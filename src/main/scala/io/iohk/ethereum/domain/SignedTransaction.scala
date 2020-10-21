package io.iohk.ethereum.domain

import akka.util.ByteString
import com.google.common.cache.{Cache, CacheBuilder}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.{ECDSASignature, kec256}
import io.iohk.ethereum.mpt.ByteArraySerializable
import io.iohk.ethereum.network.p2p.messages.PV60.SignedTransactions._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}
import monix.eval.Task
import monix.execution.Scheduler
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.util.encoders.Hex

import java.math.BigInteger
import java.util.concurrent.Executors
import scala.util.Try

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
  val negativePointSign = 27
  val newNegativePointSign = 35
  val positivePointSign = 28
  val newPositivePointSign = 36
  val valueForEmptyR = 0
  val valueForEmptyS = 0

  def apply(
      tx: Transaction,
      pointSign: Byte,
      signatureRandom: ByteString,
      signature: ByteString,
      chainId: Byte
  ): SignedTransaction = {
    val txSignature = ECDSASignature(
      r = new BigInteger(1, signatureRandom.toArray),
      s = new BigInteger(1, signature.toArray),
      v = pointSign
    )
    SignedTransaction(tx, txSignature)
  }

  def apply(tx: Transaction, pointSign: Byte, signatureRandom: ByteString, signature: ByteString): SignedTransaction = {
    val txSignature = ECDSASignature(
      r = new BigInteger(1, signatureRandom.toArray),
      s = new BigInteger(1, signature.toArray),
      v = pointSign
    )
    SignedTransaction(tx, txSignature)
  }

  def sign(tx: Transaction, keyPair: AsymmetricCipherKeyPair, chainId: Option[Byte]): SignedTransactionWithSender = {
    val bytes = bytesToSign(tx, chainId)
    val sig = ECDSASignature.sign(bytes, keyPair, chainId)
    val address = Address(keyPair)
    SignedTransactionWithSender(tx, sig, address)
  }

  private def bytesToSign(tx: Transaction, chainId: Option[Byte]): Array[Byte] = {
    chainId match {
      case Some(id) =>
        chainSpecificTransactionBytes(tx, id)
      case None =>
        generalTransactionBytes(tx)
    }
  }

  def getSender(tx: SignedTransaction): Option[Address] = {
    Option(txSenders.getIfPresent(tx.hash)) orElse calculateSender(tx)
  }

  private def calculateSender(tx: SignedTransaction): Option[Address] = Try {
    val ECDSASignature(_, _, v) = tx.signature
    val bytesToSign: Array[Byte] = if (v == ECDSASignature.negativePointSign || v == ECDSASignature.positivePointSign) {
      generalTransactionBytes(tx.tx)
    } else {
      chainSpecificTransactionBytes(tx.tx, chainId)
    }

    val recoveredPublicKey: Option[Array[Byte]] = tx.signature.publicKey(bytesToSign, Some(chainId))

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

  private def calculateSendersForTxs(txs: Seq[SignedTransaction]): Task[Unit] = {
    Task(txs.foreach(calculateAndCacheSender))
  }

  private def calculateAndCacheSender(stx: SignedTransaction) = {
    calculateSender(stx).foreach(address => txSenders.put(stx.hash, address))
  }

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

  val byteArraySerializable = new ByteArraySerializable[SignedTransaction] {

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
  lazy val hashAsHexString: String = Hex.toHexString(hash.toArray[Byte])
}

case class SignedTransactionWithSender(tx: SignedTransaction, senderAddress: Address)

object SignedTransactionWithSender {

  def getSignedTransactions(stxs: Seq[SignedTransaction]): Seq[SignedTransactionWithSender] = {
    stxs.foldLeft(List.empty[SignedTransactionWithSender]) { (acc, stx) =>
      val sender = SignedTransaction.getSender(stx)
      sender.fold(acc) { addr => SignedTransactionWithSender(stx, addr) :: acc }
    }
  }

  def apply(transaction: Transaction, signature: ECDSASignature, sender: Address): SignedTransactionWithSender = {
    SignedTransactionWithSender(SignedTransaction(transaction, signature), sender)
  }
}
