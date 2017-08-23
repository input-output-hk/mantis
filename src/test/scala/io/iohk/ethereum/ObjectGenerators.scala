package io.iohk.ethereum

import java.math.BigInteger
import java.security.SecureRandom

import akka.util.ByteString
import io.iohk.ethereum.mpt.HexPrefix.bytesToNibbles
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.vm.UInt256
import org.scalacheck.{Arbitrary, Gen}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, LeafNode, MptNode}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

import scala.collection.immutable


trait ObjectGenerators {

  def byteGen: Gen[Byte] = Gen.choose(Byte.MinValue, Byte.MaxValue)

  def shortGen: Gen[Short] = Gen.choose(Short.MinValue, Short.MaxValue)

  def intGen(min: Int, max: Int): Gen[Int] = Gen.choose(min, max)

  def intGen: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)

  def longGen: Gen[Long] = Gen.choose(Long.MinValue, Long.MaxValue)

  def bigIntGen: Gen[BigInt] = byteArrayOfNItemsGen(32).map(b => new BigInteger(1, b))

  def randomSizeByteArrayGen(minSize: Int, maxSize: Int): Gen[Array[Byte]] = Gen.choose(minSize, maxSize).flatMap(byteArrayOfNItemsGen(_))

  def byteArrayOfNItemsGen(n: Int): Gen[Array[Byte]] = Gen.listOfN(n, Arbitrary.arbitrary[Byte]).map(_.toArray)

  def randomSizeByteStringGen(minSize: Int, maxSize: Int): Gen[ByteString] = Gen.choose(minSize, maxSize).flatMap(byteStringOfLengthNGen)

  def byteStringOfLengthNGen(n: Int): Gen[ByteString] = byteArrayOfNItemsGen(n).map(ByteString(_))

  def seqByteStringOfNItemsGen(n: Int): Gen[Seq[ByteString]] = Gen.listOf(byteStringOfLengthNGen(n))

  def hexPrefixDecodeParametersGen(): Gen[(Array[Byte], Boolean)] = {
    for {
      aByteList <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
      t <- Arbitrary.arbitrary[Boolean]
    } yield (aByteList.toArray, t)
  }

  def keyValueListGen(): Gen[List[(Int, Int)]] = {
    for {
      aKeyList <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Int]).map(_.distinct)
    } yield aKeyList.zip(aKeyList)
  }

  def receiptGen(): Gen[Receipt] = for {
    postTransactionStateHash <- byteArrayOfNItemsGen(32)
    cumulativeGasUsed <- bigIntGen
    logsBloomFilter <- byteArrayOfNItemsGen(256)
  } yield Receipt(
    postTransactionStateHash = ByteString(postTransactionStateHash),
    cumulativeGasUsed = cumulativeGasUsed,
    logsBloomFilter = ByteString(logsBloomFilter),
    logs = Seq()
  )

  def transactionGen(): Gen[Transaction] = for {
    nonce <- bigIntGen
    gasPrice <- bigIntGen
    gasLimit <- bigIntGen
    receivingAddress <- byteArrayOfNItemsGen(20).map(Address(_))
    value <- bigIntGen
    payload <- byteStringOfLengthNGen(256)
  } yield Transaction(
    nonce,
    gasPrice,
    gasLimit,
    receivingAddress,
    value,
    payload
  )

  def receiptsGen(n: Int): Gen[Seq[Seq[Receipt]]] = Gen.listOfN(n, Gen.listOf(receiptGen()))

  def branchNodeGen: Gen[BranchNode] = for {
    children <- Gen.listOfN(16, byteStringOfLengthNGen(32)).map(childrenList => childrenList.map(child => Some(Left(child))))
    terminator <- byteStringOfLengthNGen(32)
  } yield BranchNode(children, Some(terminator))

  def extensionNodeGen: Gen[ExtensionNode] = for {
    keyNibbles <- byteArrayOfNItemsGen(32)
    value <- byteStringOfLengthNGen(32)
  } yield ExtensionNode(ByteString(bytesToNibbles(keyNibbles)), Left(value))

  def leafNodeGen: Gen[LeafNode] = for {
    keyNibbles <- byteArrayOfNItemsGen(32)
    value <- byteStringOfLengthNGen(32)
  } yield LeafNode(ByteString(bytesToNibbles(keyNibbles)), value)

  def nodeGen: Gen[MptNode] = Gen.choose(0, 2).flatMap{ i =>
    i match {
      case 0 => branchNodeGen
      case 1 => extensionNodeGen
      case 2 => leafNodeGen
    }
  }

  def signedTxSeqGen(length: Int, secureRandom: SecureRandom, chainId: Option[Byte]): Gen[Seq[SignedTransaction]] = {
    val senderKeys = crypto.generateKeyPair(secureRandom)
    val txsSeqGen = Gen.listOfN(length, transactionGen())
    txsSeqGen.map { txs =>
      txs.map { tx => SignedTransaction.sign(tx, senderKeys, chainId) }
    }
  }

  def newBlockGen(secureRandom: SecureRandom, chainId: Option[Byte]): Gen[NewBlock] = for {
    blockHeader <- blockHeaderGen
    stxs <- signedTxSeqGen(10, secureRandom, chainId)
    uncles <- seqBlockHeaderGen
    td <- bigIntGen
  } yield NewBlock(Block(blockHeader, BlockBody(stxs, uncles)), td)

  def blockHeaderGen: Gen[BlockHeader] = for {
    parentHash <- byteStringOfLengthNGen(32)
    ommersHash <- byteStringOfLengthNGen(32)
    beneficiary <- byteStringOfLengthNGen(20)
    stateRoot <- byteStringOfLengthNGen(32)
    transactionsRoot <- byteStringOfLengthNGen(32)
    receiptsRoot <- byteStringOfLengthNGen(32)
    logsBloom <- byteStringOfLengthNGen(50)
    difficulty <- bigIntGen
    number <- bigIntGen
    gasLimit <- bigIntGen
    gasUsed <- bigIntGen
    unixTimestamp <- intGen.map(_.abs)
    extraData <- byteStringOfLengthNGen(8)
    mixHash <- byteStringOfLengthNGen(8)
    nonce <- byteStringOfLengthNGen(8)
  } yield BlockHeader(
    parentHash = parentHash,
    ommersHash = ommersHash,
    beneficiary = beneficiary,
    stateRoot = stateRoot,
    transactionsRoot = transactionsRoot,
    receiptsRoot = receiptsRoot,
    logsBloom = logsBloom,
    difficulty = difficulty,
    number = number,
    gasLimit = gasLimit,
    gasUsed = gasUsed,
    unixTimestamp = unixTimestamp,
    extraData = extraData,
    mixHash = mixHash,
    nonce = nonce)

  def seqBlockHeaderGen: Gen[Seq[BlockHeader]] = Gen.listOf(blockHeaderGen)

}
