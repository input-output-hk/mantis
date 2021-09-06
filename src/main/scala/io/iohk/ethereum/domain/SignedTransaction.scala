package io.iohk.ethereum.domain

import java.math.BigInteger
import java.util.concurrent.Executors

import akka.util.ByteString

import monix.eval.Task
import monix.execution.Scheduler

import scala.util.Try

import com.google.common.cache.Cache
import com.google.common.cache.CacheBuilder
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.mpt.ByteArraySerializable
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.SignedTransactions._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}
import io.iohk.ethereum.utils.Config

object SignedTransaction {

  implicit private val executionContext: Scheduler = Scheduler(Executors.newWorkStealingPool())

  // txHash size is 32bytes, Address size is 20 bytes, taking into account some overhead key-val pair have
  // around 70bytes then 100k entries have around 7mb. 100k entries is around 300blocks for Ethereum network.
  val maximumSenderCacheSize = 100000

  // Each background thread gets batch of signed tx to calculate senders
  val batchSize = 5

  private val txSenders: Cache[ByteString, Address] = CacheBuilder
    .newBuilder()
    .maximumSize(maximumSenderCacheSize)
    .recordStats()
    .build()

  val FirstByteOfAddress = 12
  val LastByteOfAddress: Int = FirstByteOfAddress + Address.Length
  val EIP155NegativePointSign = 35
  val EIP155PositivePointSign = 36
  val valueForEmptyR = 0
  val valueForEmptyS = 0

  def apply(
      tx: Transaction,
      pointSign: Byte,
      signatureRandom: ByteString,
      signature: ByteString
  ): SignedTransaction = {
    val txSignature = ECDSASignature(
      r = new BigInteger(1, signatureRandom.toArray),
      s = new BigInteger(1, signature.toArray),
      v = pointSign
    )
    SignedTransaction(tx, txSignature)
  }

  def sign(
      tx: Transaction,
      keyPair: AsymmetricCipherKeyPair,
      chainId: Option[Byte]
  ): SignedTransaction = {
    val bytes = bytesToSign(tx, chainId)
    val sig = ECDSASignature.sign(bytes, keyPair /*, chainId*/ )
    SignedTransaction(tx, getEthereumSignature(sig, chainId))
  }

  private def bytesToSign(tx: Transaction, chainId: Option[Byte]): Array[Byte] =
    chainId match {
      case Some(id) =>
        chainSpecificTransactionBytes(tx, id)
      case None =>
        generalTransactionBytes(tx)
    }

  /** Convert a RLP compatible ECDSA Signature to a raw crypto signature.
    * Depending on the transaction type and the block number, different rules are
    * used to enhance the v field with additional context for signing purpose and networking
    * communication.
    *
    * Currently, both semantic data are represented by the same data structure.
    *
    * @see getEthereumSignature for the reciprocal conversion.
    * @param ethereumSignature the v-modified signature, received from the network
    * @param chainIdOpt        the chainId if available
    * @return a raw crypto signature, with only 27 or 28 as valid ECDSASignature.v value
    */
  private def getRawSignature(ethereumSignature: ECDSASignature, chainIdOpt: Option[Byte]): ECDSASignature =
    chainIdOpt match {
      // ignore chainId for unprotected negative y-parity in pre-eip155 signature
      case Some(_) if ethereumSignature.v == ECDSASignature.negativePointSign =>
        ethereumSignature.copy(v = ECDSASignature.negativePointSign)
      // ignore chainId for unprotected positive y-parity in pre-eip155 signature
      case Some(_) if ethereumSignature.v == ECDSASignature.positivePointSign =>
        ethereumSignature.copy(v = ECDSASignature.positivePointSign)
      // identify negative y-parity for protected post eip-155 signature
      case Some(chainId) if ethereumSignature.v == (2 * chainId + EIP155NegativePointSign).toByte =>
        ethereumSignature.copy(v = ECDSASignature.negativePointSign)
      // identify positive y-parity for protected post eip-155 signature
      case Some(chainId) if ethereumSignature.v == (2 * chainId + EIP155PositivePointSign).toByte =>
        ethereumSignature.copy(v = ECDSASignature.positivePointSign)
      // legacy pre-eip
      case None => ethereumSignature
      // unexpected chainId
      case _ =>
        throw new IllegalStateException(
          s"Unexpected pointSign. ChainId: ${chainIdOpt.getOrElse("None")}, ethereum.signature.v: ${ethereumSignature.v}"
        )
    }

  /** Convert a RLP compatible ECDSA Signature to a raw crypto signature.
    * Depending on the transaction type and the block number, different rules are
    * used to enhance the v field with additional context for signing purpose and networking
    * communication.
    *
    * Currently, both semantic data are represented by the same data structure.
    *
    * @see getRawSignature for the reciprocal conversion.
    * @param ethereumSignature the v-modified signature, received from the network
    * @param chainIdOpt        the chainId if available
    * @return a raw crypto signature, with only 27 or 28 as valid ECDSASignature.v value
    */
  private def getEthereumSignature(rawSignature: ECDSASignature, chainIdOpt: Option[Byte]): ECDSASignature =
    chainIdOpt match {
      case Some(chainId) if rawSignature.v == ECDSASignature.negativePointSign =>
        rawSignature.copy(v = (chainId * 2 + EIP155NegativePointSign).toByte)
      case Some(chainId) if rawSignature.v == ECDSASignature.positivePointSign =>
        rawSignature.copy(v = (chainId * 2 + EIP155PositivePointSign).toByte)
      case None => rawSignature
      case _ =>
        throw new IllegalStateException(
          s"Unexpected pointSign. ChainId: ${chainIdOpt.getOrElse("None")}, raw.signature.v: ${rawSignature.v}, authorized values are ${ECDSASignature.allowedPointSigns
            .mkString(", ")}"
        )
    }

  def getSender(tx: SignedTransaction): Option[Address] =
    Option(txSenders.getIfPresent(tx.hash)).orElse(calculateSender(tx))

  private def calculateSender(tx: SignedTransaction): Option[Address] = Try {
    val ECDSASignature(_, _, v) = tx.signature
    // chainId specific code that will be refactored with the Signer feature (ETCM-1096)
    val chainIdOpt = extractChainId(tx)
    val bytesToSign: Array[Byte] = chainIdOpt match {
      case None          => generalTransactionBytes(tx.tx)
      case Some(chainId) => chainSpecificTransactionBytes(tx.tx, chainId)
    }

    val recoveredPublicKey: Option[Array[Byte]] = getRawSignature(tx.signature, chainIdOpt).publicKey(bytesToSign)

    for {
      key <- recoveredPublicKey
      addrBytes = crypto.kec256(key).slice(FirstByteOfAddress, LastByteOfAddress)
      if addrBytes.length == Address.Length
    } yield Address(addrBytes)
  }.toOption.flatten

  def retrieveSendersInBackGround(blocks: Seq[BlockBody]): Unit = {
    val blocktx = blocks
      .collect {
        case block if block.transactionList.nonEmpty => block.transactionList
      }
      .flatten
      .grouped(batchSize)

    Task.traverse(blocktx.toSeq)(calculateSendersForTxs).runAsyncAndForget
  }

  private def calculateSendersForTxs(txs: Seq[SignedTransaction]): Task[Unit] =
    Task(txs.foreach(calculateAndCacheSender))

  private def calculateAndCacheSender(stx: SignedTransaction) =
    calculateSender(stx).foreach(address => txSenders.put(stx.hash, address))

  private def generalTransactionBytes(tx: Transaction): Array[Byte] = {
    val receivingAddressAsArray: Array[Byte] = tx.receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray)
    crypto.kec256(rlpEncode(RLPList(tx.nonce, tx.gasPrice, tx.gasLimit, receivingAddressAsArray, tx.value, tx.payload)))
  }

  private def chainSpecificTransactionBytes(tx: Transaction, chainId: Byte): Array[Byte] = {
    val receivingAddressAsArray: Array[Byte] = tx.receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray)
    crypto.kec256(
      rlpEncode(
        RLPList(
          tx.nonce,
          tx.gasPrice,
          tx.gasLimit,
          receivingAddressAsArray,
          tx.value,
          tx.payload,
          chainId,
          valueForEmptyR,
          valueForEmptyS
        )
      )
    )
  }

  private def extractChainId(stx: SignedTransaction): Option[Byte] = {
    val chainIdOpt: Option[BigInt] = stx.tx match {
      case _: LegacyTransaction
          if stx.signature.v == ECDSASignature.negativePointSign || stx.signature.v == ECDSASignature.positivePointSign =>
        None
      case _: LegacyTransaction            => Some(Config.blockchains.blockchainConfig.chainId)
      case twal: TransactionWithAccessList => Some(twal.chainId)
    }
    chainIdOpt.map(_.toByte)
  }

  val byteArraySerializable: ByteArraySerializable[SignedTransaction] = new ByteArraySerializable[SignedTransaction] {

    override def fromBytes(bytes: Array[Byte]): SignedTransaction = bytes.toSignedTransaction

    override def toBytes(input: SignedTransaction): Array[Byte] = input.toBytes
  }
}

case class SignedTransaction(tx: Transaction, signature: ECDSASignature) {

  def safeSenderIsEqualTo(address: Address): Boolean =
    SignedTransaction.getSender(this).contains(address)

  override def toString: String =
    s"SignedTransaction { " +
      s"tx: $tx, " +
      s"signature: $signature" +
      s"}"

  def isChainSpecific: Boolean =
    signature.v != ECDSASignature.negativePointSign && signature.v != ECDSASignature.positivePointSign

  lazy val hash: ByteString = ByteString(kec256(this.toBytes: Array[Byte]))
}

case class SignedTransactionWithSender(tx: SignedTransaction, senderAddress: Address)

object SignedTransactionWithSender {

  def getSignedTransactions(stxs: Seq[SignedTransaction]): Seq[SignedTransactionWithSender] =
    stxs.foldLeft(List.empty[SignedTransactionWithSender]) { (acc, stx) =>
      val sender = SignedTransaction.getSender(stx)
      sender.fold(acc)(addr => SignedTransactionWithSender(stx, addr) :: acc)
    }

  def apply(transaction: LegacyTransaction, signature: ECDSASignature, sender: Address): SignedTransactionWithSender =
    SignedTransactionWithSender(SignedTransaction(transaction, signature), sender)
}
