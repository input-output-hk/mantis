package io.iohk.ethereum.ets.blockchain

import java.util.concurrent.Executors

import akka.actor.ActorSystem
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.ets.common.TestOptions
import io.iohk.ethereum.extvm.ExtVMInterface
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.nodebuilder.VmSetup
import io.iohk.ethereum.utils.{BlockchainConfig, Config, Logger, VmConfig}
import org.scalatest._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}


object BlockchainSuite {
  implicit lazy val actorSystem = ActorSystem("mantis_system")
  implicit val testContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))
  lazy val extvm = VmSetup.vm(VmConfig(Config.config), BlockchainConfig(Config.config), testMode = true)
}

class BlockchainSuite extends FreeSpec with Matchers with BeforeAndAfterAll with Logger {
  import BlockchainSuite.testContext
  val unsupportedNetworks = Set("Constantinople")
  val supportedNetworks =
    Set("EIP150", "Frontier", "FrontierToHomesteadAt5", "Homestead", "HomesteadToEIP150At5", "HomesteadToDaoAt5", "EIP158", "Byzantium", "EIP158ToByzantiumAt5")
  //Map of ignored tests, empty set of ignored names means cancellation of whole group
  val ignoredTests: Map[String, Set[String]] = Map(
    // Tests are failing because block reward is not correctly paid to the miner
    "GeneralStateTests/stShift/sar00_d0g0v0" -> Set.empty,
    "GeneralStateTests/stShift/sar_0_256-1_d0g0v0" -> Set.empty
  )
  var vm: VMImpl = _

  override def run(testName: Option[String], args: Args): Status = {
    val options = TestOptions(args.configMap)
    val scenarios = BlockchainScenarioLoader.load("ets/BlockchainTests/", options)

    vm = if (options.useLocalVM) new VMImpl else BlockchainSuite.extvm

    scenarios.foreach { group =>
      group.name - {
        for {
          (name, scenario) <- group.scenarios
          if options.isScenarioIncluded(name)
        } {
          name in new ScenarioSetup(vm, scenario) {
            if (unsupportedNetworks.contains(scenario.network)) {
              cancel(s"Unsupported network: ${scenario.network}")
            } else if (!supportedNetworks.contains(scenario.network)) {
              fail(s"Unknown network: ${scenario.network}")
            } else if (isCanceled(group.name, name)){
              cancel(s"Test: $name in group: ${group.name} not yet supported")
            } else {
              log.info(s"Running test: ${group.name}#$name")
              runScenario(scenario, this, name)
            }
          }
        }
      }
    }

    runTests(testName, args)
  }

  override def afterAll: Unit = {
    vm match {
      case extVm: ExtVMInterface => extVm.close()
      case _ =>
    }
  }

  private def isCanceled(groupName: String, testName: String): Boolean =
    ignoredTests.get(groupName).isDefined && (ignoredTests(groupName).contains(testName) || ignoredTests(groupName).isEmpty)

  private def runScenario(scenario: BlockchainScenario, setup: ScenarioSetup, name: String): Unit = {

    import setup._

    def importBlocks(blocks: List[Block], importedBlocks: List[Block] = Nil): Future[List[Block]] = {
      if (blocks.isEmpty) {
        Future.successful(importedBlocks)
      } else {
        val blockToImport = blocks.head
        ledger.importBlock(blockToImport).flatMap {result =>
          importBlocks(blocks.tail, blockToImport :: importedBlocks)
        }
      }
    }

    loadGenesis()

    val blocksToProcess = getBlocks(scenario.blocks)

    val invalidBlocks = getBlocks(getInvalid)

    val ready = Await.result(importBlocks(blocksToProcess), Duration.Inf)

    val lastBlock = getBestBlock()

    val expectedWorldStateHash = finalWorld.stateRootHash

    lastBlock shouldBe defined

    val expectedState = getExpectedState()
    val resultState = getResultState()

    lastBlock.get.header.hash shouldEqual scenario.lastblockhash
    resultState should contain theSameElementsAs expectedState
    lastBlock.get.header.stateRoot shouldEqual expectedWorldStateHash
  }
}



