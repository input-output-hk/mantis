package io.iohk.ethereum.ets.blockchain

import akka.actor.ActorSystem
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.ets.common.TestOptions
import io.iohk.ethereum.extvm.ExtVMInterface
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.nodebuilder.VmSetup
import io.iohk.ethereum.utils.{Config, Logger, VmConfig}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{Args, BeforeAndAfterAll, Status}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

object BlockchainSuite {
  implicit lazy val actorSystem: ActorSystem = ActorSystem("mantis_system")
  implicit val testContext: Scheduler = Scheduler.fixedPool("blockchain-suite-pool", 4)
  lazy val extvm: VMImpl = VmSetup.vm(VmConfig(Config.config), Config.blockchains.blockchainConfig, testMode = true)
}

class BlockchainSuite extends AnyFreeSpec with Matchers with BeforeAndAfterAll with Logger {
  import BlockchainSuite.testContext

  val unsupportedNetworks: Set[String] = Set(
    "Berlin"
  )
  val supportedNetworks = Set(
    "EIP150",
    "Frontier",
    "FrontierToHomesteadAt5",
    "Homestead",
    "HomesteadToEIP150At5",
    "HomesteadToDaoAt5",
    "EIP158",
    "Byzantium",
    "EIP158ToByzantiumAt5",
    "Constantinople",
    "ByzantiumToConstantinopleFixAt5",
    "ConstantinopleFix",
    "Istanbul"
  )
  // Map of ignored tests, empty set of ignored names means cancellation of whole group
  val ignoredTests: Map[String, Set[String]] = Map()
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
            } else if (isCanceled(group.name, name)) {
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

  override def afterAll(): Unit = {
    vm match {
      case extVm: ExtVMInterface => extVm.close()
      case _ =>
    }
  }

  private def isCanceled(groupName: String, testName: String): Boolean =
    ignoredTests.get(groupName).isDefined && (ignoredTests(groupName).contains(testName) || ignoredTests(
      groupName
    ).isEmpty)

  private def runScenario(scenario: BlockchainScenario, setup: ScenarioSetup, name: String): Unit = {

    import setup._

    def importBlocks(blocks: List[Block], importedBlocks: List[Block] = Nil): Task[List[Block]] = {
      if (blocks.isEmpty) {
        Task.now(importedBlocks)
      } else {
        val blockToImport = blocks.head
        ledger.importBlock(blockToImport).flatMap { _ =>
          importBlocks(blocks.tail, blockToImport :: importedBlocks)
        }
      }
    }

    loadGenesis()

    val blocksToProcess = getBlocks(scenario.blocks)

    getBlocks(getInvalid)

    importBlocks(blocksToProcess).runSyncUnsafe()

    val lastBlock = getBestBlock

    val expectedWorldStateHash =
      scenario.postStateHash
        .orElse(finalWorld.map(_.stateRootHash))
        .getOrElse(throw new IllegalStateException("postState or PostStateHash not defined"))

    lastBlock shouldBe defined

    val expectedState = getExpectedState.toList.flatten
    val resultState = getResultState.toList.flatten

    lastBlock.get.header.hash shouldEqual scenario.lastblockhash
    resultState should contain theSameElementsAs expectedState
    lastBlock.get.header.stateRoot shouldEqual expectedWorldStateHash
  }
}
