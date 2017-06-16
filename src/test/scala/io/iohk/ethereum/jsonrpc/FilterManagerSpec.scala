package io.iohk.ethereum.jsonrpc

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import io.iohk.ethereum.keystore.KeyStore
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import akka.pattern.ask
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.DefaultPatience
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.jsonrpc.FilterManager.LogFilterLogs
import io.iohk.ethereum.ledger.BloomFilter
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils.FilterConfig
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._

class FilterManagerSpec extends FlatSpec with Matchers with ScalaFutures with DefaultPatience {

  implicit val timeout = Timeout(5.seconds)

  "FilterManager" should "handle log filter logs and changes" in new TestSetup {
    val filterManager = TestActorRef[FilterManager](Props(new FilterManager(blockchain, appStateStorage, keyStore, pendingTransactionsManager.ref, config, Some(time.scheduler))))

    val address = Address("0x1234")
    val topics = Seq(Seq(), Seq(ByteString(Hex.decode("4567"))))

    (appStateStorage.getBestBlockNumber _).expects().returning(3)

    val createResp =
      (filterManager ? FilterManager.NewLogFilter(Some(BlockParam.WithNumber(1)), Some(BlockParam.Latest), Some(address), topics))
        .mapTo[FilterManager.NewFilterResponse].futureValue

    val logs1 = Seq(TxLogEntry(Address("0x4567"), Nil, ByteString()))
    val bh1 = blockHeader.copy(
      number = 1,
      logsBloom = BloomFilter.create(logs1))

    val logs2 = Seq(TxLogEntry(Address("0x1234"), Seq(ByteString("can be any"), ByteString(Hex.decode("4567"))), ByteString(Hex.decode("99aaff"))))
    val bh2 = blockHeader.copy(
      number = 2,
      logsBloom = BloomFilter.create(logs2))

    val bh3 = blockHeader.copy(
      number = 3,
      logsBloom = BloomFilter.create(Nil))

    (appStateStorage.getBestBlockNumber _).expects().returning(3).twice()
    (blockchain.getBlockHeaderByNumber _).expects(BigInt(1)).returning(Some(bh1))
    (blockchain.getBlockHeaderByNumber _).expects(BigInt(2)).returning(Some(bh2))
    (blockchain.getBlockHeaderByNumber _).expects(BigInt(3)).returning(Some(bh3))

    val bb2 = BlockBody(
      transactionList = Seq(SignedTransaction(
        tx = Transaction(
          nonce = 0,
          gasPrice = 123,
          gasLimit = 123,
          receivingAddress = Address("0x1234"),
          value = 0,
          payload = ByteString()),
        signature = ECDSASignature(0, 0, 0.toByte),
        senderAddress = Address("0x0099"))),
      uncleNodesList = Nil)

    (blockchain.getBlockBodyByHash _).expects(bh2.hash).returning(Some(bb2))
    (blockchain.getReceiptsByHash _).expects(bh2.hash).returning(Some(Seq(Receipt(
      postTransactionStateHash = ByteString(),
      cumulativeGasUsed = 0,
      logsBloomFilter = BloomFilter.create(logs2),
      logs = logs2))))

    val logsResp =
      (filterManager ? FilterManager.GetFilterLogs(createResp.id))
        .mapTo[FilterManager.LogFilterLogs].futureValue

    logsResp.logs.size shouldBe 1
    logsResp.logs.head shouldBe FilterManager.Log(
      logIndex = 0,
      transactionIndex = Some(0),
      transactionHash = Some(bb2.transactionList.head.hash),
      blockHash = bh2.hash,
      blockNumber = bh2.number,
      address = Address(0x1234),
      data = ByteString(Hex.decode("99aaff")),
      topics = logs2.head.logTopics)

    // same best block, no new logs
    (appStateStorage.getBestBlockNumber _).expects().returning(3).twice()

    val changesResp1 =
      (filterManager ? FilterManager.GetFilterChanges(createResp.id))
        .mapTo[FilterManager.LogFilterChanges].futureValue

    changesResp1.logs.size shouldBe 0

    // new block with new logs
    (appStateStorage.getBestBlockNumber _).expects().returning(4).twice()

    val log4_1 = TxLogEntry(Address("0x1234"), Seq(ByteString("can be any"), ByteString(Hex.decode("4567"))), ByteString(Hex.decode("99aaff")))
    val log4_2 = TxLogEntry(Address("0x123456"), Seq(ByteString("can be any"), ByteString(Hex.decode("4567"))), ByteString(Hex.decode("99aaff"))) // address doesn't match

    val bh4 = blockHeader.copy(
      number = 4,
      logsBloom = BloomFilter.create(Seq(log4_1, log4_2)))

    (blockchain.getBlockHeaderByNumber _).expects(BigInt(4)).returning(Some(bh4))

    val bb4 = BlockBody(
      transactionList = Seq(
        SignedTransaction(
          tx = Transaction(
            nonce = 0,
            gasPrice = 123,
            gasLimit = 123,
            receivingAddress = Address("0x1234"),
            value = 0,
            payload = ByteString()),
          signature = ECDSASignature(0, 0, 0.toByte),
          senderAddress = Address("0x0099")),
        SignedTransaction(
          tx = Transaction(
            nonce = 0,
            gasPrice = 123,
            gasLimit = 123,
            receivingAddress = Address("0x123456"),
            value = 0,
            payload = ByteString()),
          signature = ECDSASignature(0, 0, 0.toByte),
          senderAddress = Address("0x0099"))),
      uncleNodesList = Nil)

    (blockchain.getBlockBodyByHash _).expects(bh4.hash).returning(Some(bb4))
    (blockchain.getReceiptsByHash _).expects(bh4.hash).returning(Some(Seq(
      Receipt(
        postTransactionStateHash = ByteString(),
        cumulativeGasUsed = 0,
        logsBloomFilter = BloomFilter.create(Seq(log4_1)),
        logs = Seq(log4_1)),
      Receipt(
        postTransactionStateHash = ByteString(),
        cumulativeGasUsed = 0,
        logsBloomFilter = BloomFilter.create(Seq(log4_2)),
        logs = Seq(log4_2)))))

    val changesResp2 =
      (filterManager ? FilterManager.GetFilterChanges(createResp.id))
        .mapTo[FilterManager.LogFilterChanges].futureValue

    changesResp2.logs.size shouldBe 1
  }

  it should "handle block filter" in new TestSetup {
    val filterManager = TestActorRef[FilterManager](Props(new FilterManager(blockchain, appStateStorage, keyStore, pendingTransactionsManager.ref, config, Some(time.scheduler))))

    (appStateStorage.getBestBlockNumber _).expects().returning(3).twice()

    val createResp =
      (filterManager ? FilterManager.NewBlockFilter())
        .mapTo[FilterManager.NewFilterResponse].futureValue

    (appStateStorage.getBestBlockNumber _).expects().returning(3)

    val getLogsRes =
      (filterManager ? FilterManager.GetFilterLogs(createResp.id))
        .mapTo[FilterManager.BlockFilterLogs].futureValue

    getLogsRes.blockHashes.size shouldBe 0

    (appStateStorage.getBestBlockNumber _).expects().returning(6)

    val bh4 = blockHeader.copy(number = 4)
    val bh5 = blockHeader.copy(number = 5)
    val bh6 = blockHeader.copy(number = 6)

    (blockchain.getBlockHeaderByNumber _).expects(BigInt(4)).returning(Some(bh4))
    (blockchain.getBlockHeaderByNumber _).expects(BigInt(5)).returning(Some(bh5))
    (blockchain.getBlockHeaderByNumber _).expects(BigInt(6)).returning(Some(bh6))

    val getChangesRes =
      (filterManager ? FilterManager.GetFilterChanges(createResp.id))
        .mapTo[FilterManager.BlockFilterChanges].futureValue

    getChangesRes.blockHashes shouldBe Seq(bh4.hash, bh5.hash, bh6.hash)
  }

  it should "handle pending transactions filter" in new TestSetup {
    val filterManager = TestActorRef[FilterManager](Props(new FilterManager(blockchain, appStateStorage, keyStore, pendingTransactionsManager.ref, config, Some(time.scheduler))))

    (appStateStorage.getBestBlockNumber _).expects().returning(3).twice()

    val createResp =
      (filterManager ? FilterManager.NewPendingTransactionFilter())
        .mapTo[FilterManager.NewFilterResponse].futureValue

    (appStateStorage.getBestBlockNumber _).expects().returning(3)

    val pendingTxs = Seq(
      SignedTransaction(
        tx = Transaction(
          nonce = 0,
          gasPrice = 123,
          gasLimit = 123,
          receivingAddress = Address("0x1234"),
          value = 0,
          payload = ByteString()),
        signature = ECDSASignature(0, 0, 0.toByte),
        senderAddress = Address("0x0099"))
    )

    (keyStore.listAccounts _).expects().returning(Right(pendingTxs.map(_.senderAddress).toList))

    val getLogsResF =
      (filterManager ? FilterManager.GetFilterLogs(createResp.id))
        .mapTo[FilterManager.PendingTransactionFilterLogs]

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsManager.PendingTransactions(pendingTxs))

    val getLogsRes = getLogsResF.futureValue

    getLogsRes.txHashes shouldBe pendingTxs.map(_.hash)
  }

  it should "timeout unused filter" in new TestSetup {
    val filterManager = TestActorRef[FilterManager](Props(new FilterManager(blockchain, appStateStorage, keyStore, pendingTransactionsManager.ref, config, Some(time.scheduler))))

    (appStateStorage.getBestBlockNumber _).expects().returning(3).twice()

    val createResp =
      (filterManager ? FilterManager.NewPendingTransactionFilter())
        .mapTo[FilterManager.NewFilterResponse].futureValue

    (appStateStorage.getBestBlockNumber _).expects().returning(3)

    val pendingTxs = Seq(
      SignedTransaction(
        tx = Transaction(
          nonce = 0,
          gasPrice = 123,
          gasLimit = 123,
          receivingAddress = Address("0x1234"),
          value = 0,
          payload = ByteString()),
        signature = ECDSASignature(0, 0, 0.toByte),
        senderAddress = Address("0x0099"))
    )

    (keyStore.listAccounts _).expects().returning(Right(pendingTxs.map(_.senderAddress).toList))

    val getLogsResF =
      (filterManager ? FilterManager.GetFilterLogs(createResp.id))
        .mapTo[FilterManager.PendingTransactionFilterLogs]

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsManager.PendingTransactions(pendingTxs))

    val getLogsRes = getLogsResF.futureValue

    // the filter should work
    getLogsRes.txHashes shouldBe pendingTxs.map(_.hash)

    time.advance(15.seconds)

    // the filter should no longer exist
    val getLogsRes2 =
      (filterManager ? FilterManager.GetFilterLogs(createResp.id))
        .mapTo[FilterManager.FilterLogs].futureValue

    pendingTransactionsManager.expectNoMsg()

    getLogsRes2 shouldBe LogFilterLogs(Nil)
  }

  trait TestSetup extends MockFactory {
    implicit val system = ActorSystem("FilterManagerSpec_System")

    val config = new FilterConfig {
      override val filterTimeout = 10.seconds
    }

    val time = new VirtualTime

    val blockchain = mock[Blockchain]
    val appStateStorage = mock[AppStateStorage]
    val keyStore = mock[KeyStore]
    val pendingTransactionsManager = TestProbe()

    val blockHeader = BlockHeader(
      parentHash = ByteString(Hex.decode("fd07e36cfaf327801e5696134b36678f6a89fb1e8f017f2411a29d0ae810ab8b")),
      ommersHash = ByteString(Hex.decode("7766c4251396a6833ccbe4be86fbda3a200dccbe6a15d80ae3de5378b1540e04")),
      beneficiary = ByteString(Hex.decode("1b7047b4338acf65be94c1a3e8c5c9338ad7d67c")),
      stateRoot = ByteString(Hex.decode("52ce0ff43d7df2cf39f8cb8832f94d2280ebe856d84d8feb7b2281d3c5cfb990")),
      transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
      logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
      difficulty = BigInt("17864037202"),
      number = 1,
      gasLimit = 5000,
      gasUsed = 0,
      unixTimestamp = 1438270431,
      extraData = ByteString(Hex.decode("426974636f696e2069732054484520426c6f636b636861696e2e")),
      mixHash = ByteString(Hex.decode("c6d695926546d3d679199303a6d1fc983fe3f09f44396619a24c4271830a7b95")),
      nonce = ByteString(Hex.decode("62bc3dca012c1b27")))
  }

}
