package io.iohk.ethereum.ets.blockchain

import io.iohk.ethereum.ets.common.TestOptions
import io.iohk.ethereum.ledger.BlockExecutionError
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.utils.Logger
import org.scalatest._


class BlockchainSuite extends FreeSpec with Matchers with Logger {

  val unsupportedNetworks = Set("Byzantium","Constantinople", "EIP158", "EIP158ToByzantiumAt5", "HomesteadToDaoAt5")
  val supportedNetworks = Set("EIP150", "Frontier", "FrontierToHomesteadAt5", "Homestead", "HomesteadToEIP150At5")

  // scalastyle:off
  override def run(testName: Option[String], args: Args): Status = {

    val options = TestOptions(args.configMap)
    val scenarios = BlockchainScenarioLoader.load("ets/BlockchainTests", options)

    scenarios.foreach { group =>
      group.name - {
        for {
          (name, scenario) <- group.scenarios
          if options.isScenarioIncluded(name)
        } {
          name in new ScenarioSetup(scenario) {
            if (unsupportedNetworks.contains(scenario.network)) {
              cancel(s"Unsupported network: ${scenario.network}")

            } else if (!supportedNetworks.contains(scenario.network)) {
              fail(s"Unknown network: ${scenario.network}")

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

  private def runScenario(scenario: BlockchainScenario, setup: ScenarioSetup): Unit = {
    import setup._

    def getBlocksToProcess() = {
      scenario.blocks.flatMap(blockDef => decodeBlock(blockDef.rlp))
    }

    val genesisBlock = loadGenesis()

    val initialWorld = getInitialWorld()

    val blocksToProcess = getBlocksToProcess()

    val newBlocks: (Seq[NewBlock], Seq[BlockExecutionError]) = processBlocks(blocksToProcess, genesisBlock.header.difficulty)

    val lastBlock = getBestBlock()

    val expectedWorldStateHash = getFinalWorld().stateRootHash

    lastBlock shouldBe defined
    lastBlock.get.header.hash shouldEqual scenario.lastblockhash
    lastBlock.get.header.stateRoot shouldEqual expectedWorldStateHash
  }

}



