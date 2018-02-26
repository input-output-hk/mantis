package io.iohk.ethereum.ets.blockchain

import akka.actor.ActorSystem
import io.iohk.ethereum.ets.common.TestOptions
import io.iohk.ethereum.nodebuilder.VmSetup
import io.iohk.ethereum.utils.{BlockchainConfig, Config, Logger, VmConfig}
import org.scalatest._

object BlockchainSuite {
  implicit lazy val actorSystem = ActorSystem("mantis_system")

  def getVmSetup(useInternal: Boolean): VmSetup = {
    val vmConfig = VmConfig(Config.config)
    val configOverride = if (useInternal) vmConfig.copy(mode = VmConfig.VmMode.Internal) else vmConfig
    new VmSetup(configOverride, BlockchainConfig(Config.config))
  }
}

class BlockchainSuite extends FreeSpec with Matchers with BeforeAndAfterAll with Logger {

  import BlockchainSuite._

  val unsupportedNetworks = Set("Byzantium","Constantinople", "EIP158ToByzantiumAt5")
  val supportedNetworks = Set("EIP150", "Frontier", "FrontierToHomesteadAt5", "Homestead", "HomesteadToEIP150At5", "HomesteadToDaoAt5", "EIP158")

  //Map of ignored tests, empty set of ignored names means cancellation of whole group
  val ignoredTests: Map[String, Set[String]] = Map()

  // None may indicate that VM connection was broken and we need to re-establish it
  var vmSetup: Option[VmSetup] = None

  override def run(testName: Option[String], args: Args): Status = {
    val options = TestOptions(args.configMap)
    val scenarios = BlockchainScenarioLoader.load("ets/BlockchainTests/", options)

    try {
      scenarios.foreach { group =>
        group.name - {
          for {
            (name, scenario) <- group.scenarios
            if options.isScenarioIncluded(name)
          } {
            if (vmSetup.isEmpty) {
              vmSetup = Some(getVmSetup(options.useLocalVM))
            }
            name in new ScenarioSetup(vmSetup.get.vm, scenario) {
              if (unsupportedNetworks.contains(scenario.network)) {
                cancel(s"Unsupported network: ${scenario.network}")
              } else if (!supportedNetworks.contains(scenario.network)) {
                fail(s"Unknown network: ${scenario.network}")
              } else if (isCanceled(group.name, name)) {
                cancel(s"Test: $name in group: ${group.name} not yet supported")
              } else {
                log.info(s"Running test: ${group.name}#$name")
                runScenario(scenario, this)
              }
            }
          }
        }
      }
    } catch { case ex: Exception => ex.printStackTrace() }

    runTests(testName, args)
  }

  override def afterAll: Unit = {
    closeVM()
  }

  private def closeVM(): Unit = {
    vmSetup.foreach(_.close())
    vmSetup = None
  }

  private def isCanceled(groupName: String, testName: String): Boolean =
    ignoredTests.get(groupName).isDefined && (ignoredTests(groupName).contains(testName) || ignoredTests(groupName).isEmpty)

  private def runScenario(scenario: BlockchainScenario, setup: ScenarioSetup): Unit = {
    import setup._

    loadGenesis()

    val blocksToProcess = getBlocks(scenario.blocks)

    val invalidBlocks = getBlocks(getInvalid)

    try {
      blocksToProcess.foreach { b =>
        val r = ledger.importBlock(b)
        log.debug(s"Block (${b.idTag}) import result: $r")
      }
      doAssertions(scenario, setup)
    } catch {
      case ex: Throwable =>
        val msg = s"Test failure due to exception: ${ex.getMessage}"
        closeVM()
        fail(msg)
    }
  }

  private def doAssertions(scenario: BlockchainScenario, setup: ScenarioSetup): Unit = {
    import setup._

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



