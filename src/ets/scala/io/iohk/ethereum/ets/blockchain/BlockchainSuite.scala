package io.iohk.ethereum.ets.blockchain

import io.iohk.ethereum.ets.common.TestOptions
import io.iohk.ethereum.ledger.BlockExecutionError
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.utils.Logger
import org.scalatest._


class BlockchainSuite extends FreeSpec with Matchers with Logger {

  val unsupportedNetworks = Set("Byzantium","Constantinople", "EIP158", "EIP158ToByzantiumAt5", "HomesteadToDaoAt5")
  val supportedNetworks = Set("EIP150", "Frontier", "FrontierToHomesteadAt5", "Homestead", "HomesteadToEIP150At5")

  val ignoredTestNames = Set(
    "bcForkUncle.json",
    "ForkStressTest.json",
    "GasLimitHigherThan2p63m1.json",
    "CallContractFromNotBestBlock.json",
    "ChainAtoChainB_blockorder1.json",
    "ChainAtoChainBtoChainA.json",
    "lotsOfBranches.json",
    "lotsOfBranchesOverrideAtTheMiddle.json",
    "lotsOfLeafs.json",
    "sideChainWithMoreTransactions.json",
    "uncleBlockAtBlock3afterBlock4.json",
    "blockChainFrontierWithLargerTDvsHomesteadBlockchain.json",
    "blockChainFrontierWithLargerTDvsHomesteadBlockchain2.json"
  )

  // scalastyle:off
  override def run(testName: Option[String], args: Args): Status = {

    val options = TestOptions(args.configMap)
    val scenarios = BlockchainScenarioLoader.load("ets/BlockchainTests/", options, ignoredTestNames)

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

    val genesisBlock = loadGenesis()

    val initialWorld = getInitialWorld()

    val blocksToProcess = getAllBlocks

    val invalidBlocks = getInvalidBlocks

    val newBlocks: (Seq[NewBlock], Seq[BlockExecutionError]) = processBlocks(blocksToProcess, genesisBlock.header.difficulty)

    // If there is more block execution errors than expected invalidblocks, it means we rejected block which should
    // pass validations, and the final state will not be correct
    if(newBlocks._2.size > invalidBlocks.size)
      newBlocks._2.foreach(err => log.info(err.toString))

    val lastBlock = getBestBlock()

    val expectedWorldStateHash = getFinalWorld().stateRootHash

    lastBlock shouldBe defined
    lastBlock.get.header.hash shouldEqual scenario.lastblockhash
    lastBlock.get.header.stateRoot shouldEqual expectedWorldStateHash
  }

}



