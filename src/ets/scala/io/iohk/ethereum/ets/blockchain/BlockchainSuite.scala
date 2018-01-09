package io.iohk.ethereum.ets.blockchain

import akka.actor.ActorSystem
import io.iohk.ethereum.ets.common.TestOptions
import io.iohk.ethereum.extvm.{ExtVMInterface, VmServerApp}
import io.iohk.ethereum.utils.{BlockchainConfig, Config, Logger}
import io.iohk.ethereum.vm.VM
import org.scalatest._

object BlockchainSuite {
  implicit lazy val actorSystem = ActorSystem("mantis_system")

  lazy val extvm = {
    import Config.config
    VmServerApp.main(Array())
    new ExtVMInterface(config.getString("extvm.host"), config.getInt("extvm.port"), BlockchainConfig(config))
  }
}

class BlockchainSuite extends FreeSpec with Matchers with BeforeAndAfterAll with Logger {

  val unsupportedNetworks = Set("Byzantium","Constantinople", "EIP158ToByzantiumAt5")
  val supportedNetworks = Set("EIP150", "Frontier", "FrontierToHomesteadAt5", "Homestead", "HomesteadToEIP150At5", "HomesteadToDaoAt5", "EIP158")

  //Map of ignored tests, empty set of ignored names means cancellation of whole group
  val ignoredTests: Map[String, Set[String]] = Map()

  var vm: VM = _

  override def run(testName: Option[String], args: Args): Status = {
    val options = TestOptions(args.configMap)
    val scenarios = BlockchainScenarioLoader.load("ets/BlockchainTests/", options)

    vm = if (options.useLocalVM) VM else BlockchainSuite.extvm

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
              log.info(s"Running test: ${group.name}/$name")
              runScenario(scenario, this)
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

  private def runScenario(scenario: BlockchainScenario, setup: ScenarioSetup): Unit = {
    import setup.{log ⇒ _, _}

    loadGenesis()

    val blocksToProcess = getBlocks(scenario.blocks)

    val invalidBlocks = getBlocks(getInvalid)

    blocksToProcess.foreach { b =>
      //val r = ledger.importBlock(b)
      try {
        val r = ledger.importBlock(b)
        log.debug(s"Block (${b.idTag}) import result: $r")
      } catch {
        case ex: Throwable =>
          ex.printStackTrace()
          println(s"WHAT A TERRIBLE FAILURE")
          sys.exit(1)
      }
    }

    val lastBlock = getBestBlock()

    val expectedWorldStateHash = finalWorld.stateRootHash

    lastBlock shouldBe defined

    val expectedState = getExpectedState()
    val resultState = getResultState()

    resultState should contain theSameElementsAs expectedState
    lastBlock.get.header.hash shouldEqual scenario.lastblockhash
    lastBlock.get.header.stateRoot shouldEqual expectedWorldStateHash
  }
}



