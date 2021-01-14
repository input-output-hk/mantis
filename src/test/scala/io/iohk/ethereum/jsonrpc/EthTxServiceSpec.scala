package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum._
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import io.iohk.ethereum.jsonrpc.EthTxService._
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager._
import io.iohk.ethereum.utils._
import io.iohk.ethereum.WithActorSystemShutDown
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import scala.concurrent.duration.Duration
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

class EthTxServiceSpec
    extends TestKit(ActorSystem("EthServiceSpec_ActorSystem"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with OptionValues
    with MockFactory
    with NormalPatience
    with TypeCheckedTripleEquals {

  it should "answer eth_getTransactionByBlockHashAndIndex with None when there is no block with the requested hash" in new TestSetup {
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2
    val request = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, txIndexToRequest)
    val response = ethTxService.getTransactionByBlockHashAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.transactionResponse shouldBe None
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with None when there is no tx in requested index" in new TestSetup {
    blockchain.storeBlock(blockToRequest).commit()

    val invalidTxIndex = blockToRequest.body.transactionList.size
    val requestWithInvalidIndex = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, invalidTxIndex)
    val response = ethTxService
      .getTransactionByBlockHashAndIndex(requestWithInvalidIndex)
      .runSyncUnsafe(Duration.Inf)
      .toOption
      .get

    response.transactionResponse shouldBe None
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with the transaction response correctly when the requested index has one" in new TestSetup {
    blockchain.storeBlock(blockToRequest).commit()

    val txIndexToRequest = blockToRequest.body.transactionList.size / 2
    val request = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, txIndexToRequest)
    val response = ethTxService.getTransactionByBlockHashAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    val requestedStx = blockToRequest.body.transactionList.apply(txIndexToRequest)
    val expectedTxResponse = TransactionResponse(requestedStx, Some(blockToRequest.header), Some(txIndexToRequest))
    response.transactionResponse shouldBe Some(expectedTxResponse)
  }

  it should "answer eth_getRawTransactionByBlockHashAndIndex with None when there is no block with the requested hash" in new TestSetup {
    // given
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2
    val request = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, txIndexToRequest)

    // when
    val response = ethTxService.getRawTransactionByBlockHashAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    // then
    response.transactionResponse shouldBe None
  }

  it should "answer eth_getRawTransactionByBlockHashAndIndex with None when there is no tx in requested index" in new TestSetup {
    // given
    blockchain.storeBlock(blockToRequest).commit()

    val invalidTxIndex = blockToRequest.body.transactionList.size
    val requestWithInvalidIndex = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, invalidTxIndex)

    // when
    val response = ethTxService
      .getRawTransactionByBlockHashAndIndex(requestWithInvalidIndex)
      .runSyncUnsafe(Duration.Inf)
      .toOption
      .value

    // then
    response.transactionResponse shouldBe None
  }

  it should "answer eth_getRawTransactionByBlockHashAndIndex with the transaction response correctly when the requested index has one" in new TestSetup {
    // given
    blockchain.storeBlock(blockToRequest).commit()
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2
    val request = GetTransactionByBlockHashAndIndexRequest(blockToRequest.header.hash, txIndexToRequest)

    // when
    val response = ethTxService.getRawTransactionByBlockHashAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    // then
    val expectedTxResponse = blockToRequest.body.transactionList.lift(txIndexToRequest)
    response.transactionResponse shouldBe expectedTxResponse
  }

  it should "handle eth_getRawTransactionByHash if the tx is not on the blockchain and not in the tx pool" in new TestSetup {
    // given
    (() => ledger.consensus).expects().returns(consensus)
    val request = GetTransactionByHashRequest(txToRequestHash)

    // when
    val response = ethTxService.getRawTransactionByHash(request).runSyncUnsafe()

    // then
    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(Nil))

    response shouldEqual Right(RawTransactionResponse(None))
  }

  it should "handle eth_getRawTransactionByHash if the tx is still pending" in new TestSetup {
    // given
    (() => ledger.consensus).expects().returns(consensus)
    val request = GetTransactionByHashRequest(txToRequestHash)

    // when
    val response = ethTxService.getRawTransactionByHash(request).runToFuture

    // then
    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(
      PendingTransactionsResponse(Seq(PendingTransaction(txToRequestWithSender, System.currentTimeMillis)))
    )

    response.futureValue shouldEqual Right(RawTransactionResponse(Some(txToRequest)))
  }

  it should "handle eth_getRawTransactionByHash if the tx was already executed" in new TestSetup {
    // given
    (() => ledger.consensus).expects().returns(consensus)

    val blockWithTx = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    blockchain.storeBlock(blockWithTx).commit()
    val request = GetTransactionByHashRequest(txToRequestHash)

    // when
    val response = ethTxService.getRawTransactionByHash(request).runSyncUnsafe()

    // then
    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(Nil))

    response shouldEqual Right(RawTransactionResponse(Some(txToRequest)))
  }

  it should "return 0 gas price if there are no transactions" in new TestSetup {
    (appStateStorage.getBestBlockNumber _).expects().returning(42)

    val response = ethTxService.getGetGasPrice(GetGasPriceRequest())
    response.runSyncUnsafe() shouldEqual Right(GetGasPriceResponse(0))
  }

  it should "return average gas price" in new TestSetup {
    blockchain.saveBestKnownBlocks(42)
    blockchain
      .storeBlock(Block(Fixtures.Blocks.Block3125369.header.copy(number = 42), Fixtures.Blocks.Block3125369.body))
      .commit()

    val response = ethTxService.getGetGasPrice(GetGasPriceRequest())
    response.runSyncUnsafe() shouldEqual Right(GetGasPriceResponse(BigInt("20000000000")))
  }

  it should "getTransactionByBlockNumberAndIndexRequest return transaction by index" in new TestSetup {
    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val txIndex: Int = 1
    val request = GetTransactionByBlockNumberAndIndexRequest(BlockParam.Latest, txIndex)
    val response = ethTxService.getTransactionByBlockNumberAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    val expectedTxResponse =
      TransactionResponse(blockToRequest.body.transactionList(txIndex), Some(blockToRequest.header), Some(txIndex))
    response.transactionResponse shouldBe Some(expectedTxResponse)
  }

  it should "getTransactionByBlockNumberAndIndexRequest return empty response if transaction does not exists when getting by index" in new TestSetup {
    blockchain.storeBlock(blockToRequest).commit()

    val txIndex: Int = blockToRequest.body.transactionList.length + 42
    val request =
      GetTransactionByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequest.header.number), txIndex)
    val response = ethTxService.getTransactionByBlockNumberAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.transactionResponse shouldBe None
  }

  it should "getTransactionByBlockNumberAndIndex return empty response if block does not exists when getting by index" in new TestSetup {
    blockchain.storeBlock(blockToRequest).commit()

    val txIndex: Int = 1
    val request =
      GetTransactionByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequest.header.number - 42), txIndex)
    val response = ethTxService.getTransactionByBlockNumberAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.transactionResponse shouldBe None
  }

  it should "getRawTransactionByBlockNumberAndIndex return transaction by index" in new TestSetup {
    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val txIndex: Int = 1
    val request = GetTransactionByBlockNumberAndIndexRequest(BlockParam.Latest, txIndex)
    val response = ethTxService.getRawTransactionByBlockNumberAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    val expectedTxResponse = blockToRequest.body.transactionList.lift(txIndex)
    response.transactionResponse shouldBe expectedTxResponse
  }

  it should "getRawTransactionByBlockNumberAndIndex return empty response if transaction does not exists when getting by index" in new TestSetup {
    blockchain.storeBlock(blockToRequest).commit()

    val txIndex: Int = blockToRequest.body.transactionList.length + 42
    val request =
      GetTransactionByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequest.header.number), txIndex)
    val response = ethTxService.getRawTransactionByBlockNumberAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.transactionResponse shouldBe None
  }

  it should "getRawTransactionByBlockNumberAndIndex return empty response if block does not exists when getting by index" in new TestSetup {
    blockchain.storeBlock(blockToRequest).commit()

    val txIndex: Int = 1
    val request =
      GetTransactionByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequest.header.number - 42), txIndex)
    val response = ethTxService.getRawTransactionByBlockNumberAndIndex(request).runSyncUnsafe(Duration.Inf).toOption.get

    response.transactionResponse shouldBe None
  }

  it should "handle get transaction by hash if the tx is not on the blockchain and not in the tx pool" in new TestSetup {
    (() => ledger.consensus).expects().returns(consensus)

    val request = GetTransactionByHashRequest(txToRequestHash)
    val response = ethTxService.getTransactionByHash(request).runSyncUnsafe()

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(Nil))

    response shouldEqual Right(GetTransactionByHashResponse(None))
  }

  it should "handle get transaction by hash if the tx is still pending" in new TestSetup {
    (() => ledger.consensus).expects().returns(consensus)

    val request = GetTransactionByHashRequest(txToRequestHash)
    val response = ethTxService.getTransactionByHash(request).runToFuture

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(
      PendingTransactionsResponse(Seq(PendingTransaction(txToRequestWithSender, System.currentTimeMillis)))
    )

    response.futureValue shouldEqual Right(GetTransactionByHashResponse(Some(TransactionResponse(txToRequest))))
  }

  it should "handle get transaction by hash if the tx was already executed" in new TestSetup {
    (() => ledger.consensus).expects().returns(consensus)

    val blockWithTx = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    blockchain.storeBlock(blockWithTx).commit()

    val request = GetTransactionByHashRequest(txToRequestHash)
    val response = ethTxService.getTransactionByHash(request).runSyncUnsafe()

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(Nil))

    response shouldEqual Right(
      GetTransactionByHashResponse(Some(TransactionResponse(txToRequest, Some(blockWithTx.header), Some(0))))
    )
  }

  it should "calculate correct contract address for contract creating by transaction" in new TestSetup {
    val body = BlockBody(Seq(Fixtures.Blocks.Block3125369.body.transactionList.head, contractCreatingTransaction), Nil)
    val blockWithTx = Block(Fixtures.Blocks.Block3125369.header, body)
    val gasUsedByTx = 4242
    blockchain
      .storeBlock(blockWithTx)
      .and(
        blockchain.storeReceipts(
          Fixtures.Blocks.Block3125369.header.hash,
          Seq(fakeReceipt, fakeReceipt.copy(cumulativeGasUsed = fakeReceipt.cumulativeGasUsed + gasUsedByTx))
        )
      )
      .commit()

    val request = GetTransactionReceiptRequest(contractCreatingTransaction.hash)
    val response = ethTxService.getTransactionReceipt(request)

    response.runSyncUnsafe() shouldEqual Right(
      GetTransactionReceiptResponse(
        Some(
          TransactionReceiptResponse(
            receipt = fakeReceipt.copy(cumulativeGasUsed = fakeReceipt.cumulativeGasUsed + gasUsedByTx),
            stx = contractCreatingTransaction,
            signedTransactionSender = contractCreatingTransactionSender,
            transactionIndex = 1,
            blockHeader = Fixtures.Blocks.Block3125369.header,
            gasUsedByTransaction = gasUsedByTx
          )
        )
      )
    )
  }

  it should "send message to pendingTransactionsManager and return an empty GetPendingTransactionsResponse" in new TestSetup {
    val res = ethTxService.getTransactionsFromPool.runSyncUnsafe()

    pendingTransactionsManager.expectMsg(GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(Nil))

    res shouldBe PendingTransactionsResponse(Nil)
  }

  it should "send message to pendingTransactionsManager and return GetPendingTransactionsResponse with two transactions" in new TestSetup {
    val transactions = (0 to 1)
      .map(_ => {
        val fakeTransaction = SignedTransactionWithSender(
          Transaction(
            nonce = 0,
            gasPrice = 123,
            gasLimit = 123,
            receivingAddress = Address("0x1234"),
            value = 0,
            payload = ByteString()
          ),
          signature = ECDSASignature(0, 0, 0.toByte),
          sender = Address("0x1234")
        )
        PendingTransaction(fakeTransaction, System.currentTimeMillis)
      })
      .toList

    val res = ethTxService.getTransactionsFromPool.runToFuture

    pendingTransactionsManager.expectMsg(GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(transactions))

    res.futureValue shouldBe PendingTransactionsResponse(transactions)
  }

  it should "send message to pendingTransactionsManager and return an empty GetPendingTransactionsResponse in case of error" in new TestSetup {
    val res = ethTxService.getTransactionsFromPool.runSyncUnsafe()

    pendingTransactionsManager.expectMsg(GetPendingTransactions)
    pendingTransactionsManager.reply(new ClassCastException("error"))

    res shouldBe PendingTransactionsResponse(Nil)
  }

  // NOTE TestSetup uses Ethash consensus; check `consensusConfig`.
  class TestSetup(implicit system: ActorSystem) extends MockFactory with EphemBlockchainTestSetup with ApisBuilder {
    val appStateStorage = mock[AppStateStorage]
    override lazy val ledger = mock[Ledger]
    val pendingTransactionsManager = TestProbe()
    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    lazy val ethTxService = new EthTxService(
      blockchain,
      ledger,
      pendingTransactionsManager.ref,
      getTransactionFromPoolTimeout
    )

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)

    val v: Byte = 0x1c
    val r = ByteString(Hex.decode("b3493e863e48a8d67572910933114a4c0e49dac0cb199e01df1575f35141a881"))
    val s = ByteString(Hex.decode("5ba423ae55087e013686f89ad71a449093745f7edb4eb39f30acd30a8964522d"))

    val payload = ByteString(
      Hex.decode(
        "60606040526040516101e43803806101e483398101604052808051820191906020018051906020019091908051" +
          "9060200190919050505b805b83835b600060018351016001600050819055503373ffffffffffffffffffffffff" +
          "ffffffffffffffff16600260005060016101008110156100025790900160005b50819055506001610102600050" +
          "60003373ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050819055" +
          "50600090505b82518110156101655782818151811015610002579060200190602002015173ffffffffffffffff" +
          "ffffffffffffffffffffffff166002600050826002016101008110156100025790900160005b50819055508060" +
          "0201610102600050600085848151811015610002579060200190602002015173ffffffffffffffffffffffffff" +
          "ffffffffffffff168152602001908152602001600020600050819055505b80600101905080506100b9565b8160" +
          "00600050819055505b50505080610105600050819055506101866101a3565b610107600050819055505b505b50" +
          "5050602f806101b56000396000f35b600062015180420490506101b2565b905636600080376020600036600073" +
          "6ab9dd83108698b9ca8d03af3c7eb91c0e54c3fc60325a03f41560015760206000f30000000000000000000000" +
          "000000000000000000000000000000000000000060000000000000000000000000000000000000000000000000" +
          "000000000000000200000000000000000000000000000000000000000000000000000000000000000000000000" +
          "0000000000000000000000000000000000000000000000000000020000000000000000000000006c9fbd9a7f06" +
          "d62ce37db2ab1e1b0c288edc797a000000000000000000000000c482d695f42b07e0d6a22925d7e49b46fd9a3f80"
      )
    )

    // //tx 0xb7b8cc9154896b25839ede4cd0c2ad193adf06489fdd9c0a9dfce05620c04ec1
    val contractCreatingTransaction: SignedTransaction = SignedTransaction(
      Transaction(
        nonce = 2550,
        gasPrice = BigInt("20000000000"),
        gasLimit = 3000000,
        receivingAddress = None,
        value = 0,
        payload
      ),
      v,
      r,
      s,
      0x3d.toByte
    )

    val contractCreatingTransactionSender = SignedTransaction.getSender(contractCreatingTransaction).get

    val fakeReceipt = Receipt.withHashOutcome(
      postTransactionStateHash = ByteString(Hex.decode("01" * 32)),
      cumulativeGasUsed = 43,
      logsBloomFilter = ByteString(Hex.decode("00" * 256)),
      logs = Seq(TxLogEntry(Address(42), Seq(ByteString(Hex.decode("01" * 32))), ByteString(Hex.decode("03" * 32))))
    )

    val txToRequest = Fixtures.Blocks.Block3125369.body.transactionList.head
    val txSender = SignedTransaction.getSender(txToRequest).get
    val txToRequestWithSender = SignedTransactionWithSender(txToRequest, txSender)

    val txToRequestHash = txToRequest.hash
  }

}
