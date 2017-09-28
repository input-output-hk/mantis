package io.iohk.ethereum.ets.blockchain

import io.iohk.ethereum.ets.common.TestOptions
import io.iohk.ethereum.ledger.BlockExecutionError
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.utils.Logger
import org.scalatest._


class BlockchainSuite extends FreeSpec with Matchers with Logger {

  val unsupportedNetworks = Set("Byzantium","Constantinople", "EIP158ToByzantiumAt5")
  val supportedNetworks = Set("EIP150", "Frontier", "FrontierToHomesteadAt5", "Homestead", "HomesteadToEIP150At5", "HomesteadToDaoAt5", "EIP158")

  //Map of ignored tests, empty set of ignored names means cancellation of whole group
  val ignoredTests: Map[String, Set[String]] = Map(
    "bcForgedTest/bcForkUncle" -> Set("ForkUncle"),
    "bcForkStressTest/ForkStressTest"  -> Set.empty,
    "bcMultiChainTest/CallContractFromNotBestBlock" -> Set.empty,
    "bcMultiChainTest/ChainAtoChainB_blockorder1" -> Set.empty,
    "bcMultiChainTest/ChainAtoChainB_blockorder2" -> Set.empty,
    "bcMultiChainTest/ChainAtoChainBtoChainA" -> Set.empty,
    "bcMultiChainTest/UncleFromSideChain" -> Set.empty,
    "bcTotalDifficultyTest/lotsOfBranches" -> Set.empty,
    "bcTotalDifficultyTest/lotsOfBranchesOverrideAtTheMiddle"  -> Set.empty,
    "bcTotalDifficultyTest/lotsOfLeafs"  -> Set.empty,
    "bcTotalDifficultyTest/sideChainWithMoreTransactions" -> Set.empty,
    "bcTotalDifficultyTest/uncleBlockAtBlock3afterBlock4" -> Set.empty,
    "TransitionTests/bcFrontierToHomestead/blockChainFrontierWithLargerTDvsHomesteadBlockchain"  -> Set.empty,
    "TransitionTests/bcFrontierToHomestead/blockChainFrontierWithLargerTDvsHomesteadBlockchain2"  -> Set.empty,
    "TransitionTests/bcFrontierToHomestead/HomesteadOverrideFrontier" -> Set.empty,
    // bcHomesteadToDao test are temporarily disabled until the update of Blockchain test suite, because
    // they are failing on older version and success on fresh on. Blockchain test suite should be updated
    // after introduction of EIP684.
    "TransitionTests/bcHomesteadToDao/DaoTransactions" -> Set.empty,
    "TransitionTests/bcHomesteadToDao/DaoTransactions_EmptyTransactionAndForkBlocksAhead" -> Set.empty,
    "TransitionTests/bcHomesteadToDao/DaoTransactions_UncleExtradata" -> Set.empty,
    "TransitionTests/bcHomesteadToDao/DaoTransactions_XBlockm1" -> Set.empty
  )
  override def run(testName: Option[String], args: Args): Status = {
    val options = TestOptions(args.configMap)
    val scenarios = BlockchainScenarioLoader.load("ets/BlockchainTests/", options)

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

  private def isCanceled(groupName: String, testName: String): Boolean =
    ignoredTests.get(groupName).isDefined && (ignoredTests(groupName).contains(testName) || ignoredTests(groupName).isEmpty)

  private def runScenario(scenario: BlockchainScenario, setup: ScenarioSetup): Unit = {
    import setup._

    loadGenesis()

    val blocksToProcess = getBlocks(scenario.blocks)

    val invalidBlocks = getBlocks(getInvalid)

    val newBlocksErrors: Seq[BlockExecutionError] = processBlocks(blocksToProcess)

    // If there is more block execution errors than expected invalidblocks, it means we rejected block which should
    // pass validations, and the final state will not be correct
    if(newBlocksErrors.size > invalidBlocks.size)
      newBlocksErrors.foreach(err => log.info(err.toString))

    newBlocksErrors.size shouldEqual invalidBlocks.size

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



