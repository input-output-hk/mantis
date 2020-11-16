package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator
import io.iohk.ethereum.domain.{Account, Address, Block, EthereumUInt256Mpt, UInt256}
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.{Ledger, StxLedger}
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.utils._
import io.iohk.ethereum._
import monix.execution.Scheduler.Implicits.global
import org.bouncycastle.util.encoders.Hex
import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class EthGetProofSpec
    extends TestKit(ActorSystem("EthGetProofSpec_ActorSystem"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with OptionValues
    with MockFactory
    with NormalPatience
    with TypeCheckedTripleEquals {

  "EthGetProof" should "handle getStorageAt request" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val storageMpt = EthereumUInt256Mpt
      .storageMpt(
        ByteString(MerklePatriciaTrie.EmptyRootHash),
        storagesInstance.storages.stateStorage.getBackingStorage(0)
      )
      .put(UInt256(333), UInt256(123))

    val account = Account(
      nonce = 0,
      balance = UInt256(0),
      storageRoot = ByteString(storageMpt.getRootHash),
      codeHash = ByteString("")
    )
    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](storagesInstance.storages.stateStorage.getBackingStorage(0))
        .put(
          crypto.kec256(address.bytes.toArray[Byte]),
          account
        )

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body)
    val newBlockHeader = blockToRequest.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newblock = blockToRequest.copy(header = newBlockHeader)
    blockchain.storeBlock(newblock).commit()
    blockchain.saveBestKnownBlocks(newblock.header.number)

    val ethGetProof = new EthGetProof(blockchain, blockGenerator, blockchainConfig.ethCompatibleStorage)
    val storageKeys: Seq[StorageProofKey] =
      Seq(StorageProofKey(333))
    val blockNumber: BlockParam = BlockParam.Latest
    val result = ethGetProof.run(address, storageKeys, blockNumber)

    val balanceResponse: GetBalanceResponse = ethService
      .getBalance(GetBalanceRequest(address, BlockParam.Latest))
      .runSyncUnsafe()
      .getOrElse(fail("ethService.getBalance did not get valid response"))

    val nonceResponse = ethService
      .getTransactionCount(GetTransactionCountRequest(address, BlockParam.Latest))
      .runSyncUnsafe()
      .getOrElse(fail("ethService.getTransactionCount did not get valid response"))

    val expected = ProofAccount(
      address = address,
      accountProof = Vector(ByteString()),
      balance = balanceResponse.value,
      codeHash = account.codeHash,
      nonce = UInt256(nonceResponse.value),
      storageHash = account.storageRoot,
      storageProof = List(
        StorageProof(
          key = StorageProofKey(333),
          value = 123,
          proof = Seq(ByteString(123))
        )
      )
    )

    result.runSyncUnsafe() shouldBe Right(expected)
  }

  class TestSetup(implicit system: ActorSystem) extends MockFactory with EphemBlockchainTestSetup with ApisBuilder {
    val blockGenerator = mock[EthashBlockGenerator]
    val keyStore = mock[KeyStore]
    override lazy val ledger = mock[Ledger]
    override lazy val stxLedger = mock[StxLedger]

    override lazy val consensus: TestConsensus = buildTestConsensus().withBlockGenerator(blockGenerator)

    val syncingController = TestProbe()
    val pendingTransactionsManager = TestProbe()
    val ommersPool = TestProbe()
    val filterManager = TestProbe()

    override lazy val consensusConfig = ConsensusConfigs.consensusConfig
    val getTransactionFromPoolTimeout: FiniteDuration = 5.seconds

    val filterConfig = new FilterConfig {
      override val filterTimeout: FiniteDuration = Timeouts.normalTimeout
      override val filterManagerQueryTimeout: FiniteDuration = Timeouts.normalTimeout
    }

    val currentProtocolVersion = 11

    val jsonRpcConfig = JsonRpcConfig(Config.config, available)

    val ethService = new EthService(
      blockchain,
      ledger,
      stxLedger,
      keyStore,
      pendingTransactionsManager.ref,
      syncingController.ref,
      ommersPool.ref,
      filterManager.ref,
      filterConfig,
      blockchainConfig,
      currentProtocolVersion,
      jsonRpcConfig,
      getTransactionFromPoolTimeout,
      Timeouts.shortTimeout
    )
  }
}
