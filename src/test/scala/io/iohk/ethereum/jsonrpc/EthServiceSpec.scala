package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum._
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.blockchain.sync.{EphemBlockchainTestSetup, SyncProtocol}
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.consensus.ethash.blocks.{EthashBlockGenerator, RestrictedEthashBlockGeneratorImpl}
import io.iohk.ethereum.consensus.ethash.difficulty.EthashDifficultyCalculator
import io.iohk.ethereum.crypto.{ECDSASignature, kec256}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.BlockHeader.getEncodedWithoutNonce
import io.iohk.ethereum.domain.{Address, Block, BlockHeader, BlockchainImpl, UInt256, _}
import io.iohk.ethereum.jsonrpc.EthService.{ProtocolVersionRequest, _}
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.Ledger.TxResult
import io.iohk.ethereum.ledger.{Ledger, StxLedger}
import io.iohk.ethereum.mpt.{ByteArrayEncoder, ByteArraySerializable, MerklePatriciaTrie}
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.testing.ActorsTesting.simpleAutoPilot
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.{
  GetPendingTransactions,
  PendingTransaction,
  PendingTransactionsResponse
}
import io.iohk.ethereum.utils._
import monix.execution.Scheduler.Implicits.global
import org.bouncycastle.util.encoders.Hex
import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}

// scalastyle:off file.size.limit
class EthServiceSpec
    extends TestKit(ActorSystem("EthServiceSpec_ActorSystem"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with OptionValues
    with MockFactory
    with NormalPatience
    with TypeCheckedTripleEquals {

  "EthService" should "return ethereum protocol version" in new TestSetup {
    val response = ethService.protocolVersion(ProtocolVersionRequest()).runSyncUnsafe()
    val protocolVersion = response.toOption.get.value

    Integer.parseInt(protocolVersion.drop(2), 16) shouldEqual currentProtocolVersion
  }

  it should "return syncing info if the peer is syncing" in new TestSetup {
    syncingController.setAutoPilot(simpleAutoPilot { case SyncProtocol.GetStatus =>
      SyncProtocol.Status.Syncing(999, Progress(200, 10000), Some(Progress(100, 144)))
    })

    val response = ethService.syncing(SyncingRequest()).runSyncUnsafe().toOption.get

    response shouldEqual SyncingResponse(
      Some(
        EthService.SyncingStatus(
          startingBlock = 999,
          currentBlock = 200,
          highestBlock = 10000,
          knownStates = 144,
          pulledStates = 100
        )
      )
    )
  }

  // scalastyle:off magic.number
  it should "return no syncing info if the peer is not syncing" in new TestSetup {
    syncingController.setAutoPilot(simpleAutoPilot { case SyncProtocol.GetStatus =>
      SyncProtocol.Status.NotSyncing
    })

    val response = ethService.syncing(SyncingRequest()).runSyncUnsafe()

    response shouldEqual Right(SyncingResponse(None))
  }

  it should "return no syncing info if sync is done" in new TestSetup {
    syncingController.setAutoPilot(simpleAutoPilot { case SyncProtocol.GetStatus =>
      SyncProtocol.Status.SyncDone
    })

    val response = ethService.syncing(SyncingRequest()).runSyncUnsafe()

    response shouldEqual Right(SyncingResponse(None))
  }

  it should "execute call and return a value" in new TestSetup {
    (() => ledger.consensus).expects().returns(consensus)
    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val txResult = TxResult(
      BlockchainImpl(storagesInstance.storages)
        .getWorldStateProxy(-1, UInt256.Zero, ByteString.empty, noEmptyAccounts = false, ethCompatibleStorage = true),
      123,
      Nil,
      ByteString("return_value"),
      None
    )
    (stxLedger.simulateTransaction _).expects(*, *, *).returning(txResult)

    val tx = CallTx(
      Some(ByteString(Hex.decode("da714fe079751fa7a1ad80b76571ea6ec52a446c"))),
      Some(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477"))),
      Some(1),
      2,
      3,
      ByteString("")
    )
    val response = ethService.call(CallRequest(tx, BlockParam.Latest))

    response.runSyncUnsafe() shouldEqual Right(CallResponse(ByteString("return_value")))
  }

  it should "execute estimateGas and return a value" in new TestSetup {
    (() => ledger.consensus).expects().returns(consensus)
    blockchain.storeBlock(blockToRequest).commit()
    blockchain.saveBestKnownBlocks(blockToRequest.header.number)

    val estimatedGas = BigInt(123)
    (stxLedger.binarySearchGasEstimation _).expects(*, *, *).returning(estimatedGas)

    val tx = CallTx(
      Some(ByteString(Hex.decode("da714fe079751fa7a1ad80b76571ea6ec52a446c"))),
      Some(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477"))),
      Some(1),
      2,
      3,
      ByteString("")
    )
    val response = ethService.estimateGas(CallRequest(tx, BlockParam.Latest))

    response.runSyncUnsafe() shouldEqual Right(EstimateGasResponse(123))
  }

  // NOTE TestSetup uses Ethash consensus; check `consensusConfig`.
  class TestSetup(implicit system: ActorSystem) extends MockFactory with EphemBlockchainTestSetup with ApisBuilder {
    val blockGenerator = mock[EthashBlockGenerator]
    val appStateStorage = mock[AppStateStorage]
    val keyStore = mock[KeyStore]
    override lazy val ledger = mock[Ledger]
    override lazy val stxLedger = mock[StxLedger]

    override lazy val consensus: TestConsensus = buildTestConsensus().withBlockGenerator(blockGenerator)

    val syncingController = TestProbe()
    val pendingTransactionsManager = TestProbe()
    val filterManager = TestProbe()

    override lazy val consensusConfig = ConsensusConfigs.consensusConfig
    val minerActiveTimeout: FiniteDuration = 5.seconds
    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    val filterConfig = new FilterConfig {
      override val filterTimeout: FiniteDuration = Timeouts.normalTimeout
      override val filterManagerQueryTimeout: FiniteDuration = Timeouts.normalTimeout
    }

    val currentProtocolVersion = 11

    lazy val ethService = new EthService(
      blockchain,
      ledger,
      stxLedger,
      keyStore,
      pendingTransactionsManager.ref,
      syncingController.ref,
      filterManager.ref,
      filterConfig,
      blockchainConfig,
      currentProtocolVersion,
      getTransactionFromPoolTimeout,
      Timeouts.shortTimeout
    )

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)

    val uncle = Fixtures.Blocks.DaoForkBlock.header

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

    //tx 0xb7b8cc9154896b25839ede4cd0c2ad193adf06489fdd9c0a9dfce05620c04ec1
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
