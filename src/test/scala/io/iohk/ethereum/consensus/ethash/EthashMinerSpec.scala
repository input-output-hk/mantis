package io.iohk.ethereum.consensus
package ethash


import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActor, TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.{RegularSync, ScenarioSetup}
import io.iohk.ethereum.consensus.blocks.PendingBlock
import io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.consensus.validators.std.StdBlockHeaderValidator
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService
import io.iohk.ethereum.jsonrpc.EthService.SubmitHashRateResponse
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.vm.VM
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers, Tag}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.Future
import scala.concurrent.duration._

class EthashMinerSpec extends FlatSpec with Matchers {
  final val EthashMinerSpecTag = Tag("EthashMinerSpec")

  "EthashMiner" should "mine valid blocks" taggedAs(EthashMinerSpecTag) in new TestSetup {
    val parent = origin
    val bfm = blockForMining(parent.header)

    (blockchain.getBestBlock _).expects().returns(parent).anyNumberOfTimes()
    (ethService.submitHashRate _).expects(*).returns(Future.successful(Right(SubmitHashRateResponse(true)))).atLeastOnce()
    (blockGenerator.generateBlockForMining _).expects(parent, Nil, consensusConfig.coinbase, Nil).returning(Right(PendingBlock(bfm, Nil))).atLeastOnce()

    ommersPool.setAutoPilot(new TestActor.AutoPilot {
      def run(sender: ActorRef, msg: Any): TestActor.AutoPilot = {
        sender ! OmmersPool.Ommers(Nil)
        TestActor.KeepRunning
      }
    })

    pendingTransactionsManager.setAutoPilot(new TestActor.AutoPilot {
      def run(sender: ActorRef, msg: Any): TestActor.AutoPilot = {
        sender ! PendingTransactionsManager.PendingTransactionsResponse(Nil)
        TestActor.KeepRunning
      }
    })

    miner ! EthashMiner.StartMining

    val block = waitForMinedBlock()

    miner ! EthashMiner.StopMining

    block.body.transactionList shouldBe Seq(txToMine)
    block.header.nonce.length shouldBe 8
    blockHeaderValidator.validate(block.header, parent.header) shouldBe Right(BlockHeaderValid)
  }

  trait TestSetup extends ScenarioSetup with MockFactory {

    val origin = Block(
      BlockHeader(
        parentHash = ByteString(Hex.decode("0000000000000000000000000000000000000000000000000000000000000000")),
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("0000000000000000000000000000000000000000")),
        stateRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        logsBloom = ByteString(Hex.decode("00" * 256)),
        difficulty = UInt256(Hex.decode("0400")).toBigInt,
        number = 0,
        gasLimit = UInt256(Hex.decode("ff1388")).toBigInt,
        gasUsed = 0,
        unixTimestamp = 0,
        extraData = ByteString(Hex.decode("00")),
        mixHash = ByteString(Hex.decode("00" * 32)),
        nonce = ByteString(Hex.decode("0000000000000042"))
      ),
      BlockBody(Seq(), Seq()))

    val blockGenerator: EthashBlockGenerator = mock[EthashBlockGenerator]

    override lazy val blockchain: BlockchainImpl = mock[BlockchainImpl]
    override lazy val vm: VM = VM
    override lazy val consensus: EthashConsensus = loadEthashConsensus().withBlockGenerator(blockGenerator)

    val difficultyCalc = new DifficultyCalculator(blockchainConfig)

    val blockForMiningTimestamp = System.currentTimeMillis()

    private def calculateGasLimit(parentGas: BigInt): BigInt = {
      val GasLimitBoundDivisor: Int = 1024

      val gasLimitDifference = parentGas / GasLimitBoundDivisor
      parentGas + gasLimitDifference - 1
    }

    val txToMine = SignedTransaction(
      tx = Transaction(
        nonce = BigInt("438553"),
        gasPrice = BigInt("20000000000"),
        gasLimit = BigInt("50000"),
        receivingAddress = Address(ByteString(Hex.decode("3435be928d783b7c48a2c3109cba0d97d680747a"))),
        value = BigInt("108516826677274384"),
        payload = ByteString.empty
      ),
      pointSign = 0x9d.toByte,
      signatureRandom = ByteString(Hex.decode("beb8226bdb90216ca29967871a6663b56bdd7b86cf3788796b52fd1ea3606698")),
      signature = ByteString(Hex.decode("2446994156bc1780cb5806e730b171b38307d5de5b9b0d9ad1f9de82e00316b5")),
      chainId = 0x3d.toByte
    ).get

    def blockForMining(parent: BlockHeader): Block = {
      Block(BlockHeader(
        parentHash = parent.hash,
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = consensusConfig.coinbase.bytes,
        stateRoot = parent.stateRoot,
        transactionsRoot = parent.transactionsRoot,
        receiptsRoot = parent.receiptsRoot,
        logsBloom = parent.logsBloom,
        difficulty = difficultyCalc.calculateDifficulty(1, blockForMiningTimestamp, parent),
        number = BigInt(1),
        gasLimit = calculateGasLimit(parent.gasLimit),
        gasUsed = BigInt(0),
        unixTimestamp = blockForMiningTimestamp,
        extraData = consensusConfig.headerExtraData,
        mixHash = ByteString.empty,
        nonce = ByteString.empty
      ), BlockBody(Seq(txToMine), Nil))
    }

    val blockHeaderValidator = new StdBlockHeaderValidator(blockchainConfig)

    implicit val system = ActorSystem("MinerSpec_System")

    val ommersPool = TestProbe()
    val pendingTransactionsManager = TestProbe()
    val syncController = TestProbe()

    val ethService = mock[EthService]

    val miner = TestActorRef(EthashMiner.props(
      blockchain, ommersPool.ref, pendingTransactionsManager.ref,
      syncController.ref, ethService, consensus))

    def waitForMinedBlock(): Block = {
      syncController.expectMsgPF[Block](10.minutes) {
        case m: RegularSync.MinedBlock => m.block
      }
    }

  }
}
