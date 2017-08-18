package io.iohk.ethereum.ets.blockchain

import io.iohk.ethereum.domain.Block.BlockDec
import io.iohk.ethereum.ets.common.TestOptions
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.utils.Logger
import org.scalatest._


class BlockchainSuite extends FreeSpec with Matchers with Logger {

  val unsupportedNetworks = Set("Byzantium","Constantinople", "EIP158", "EIP158ToByzantiumAt5", "HomesteadToDaoAt5")
  val supportedNetworks = Set("EIP150", "Frontier", "FrontierToHomesteadAt5", "Homestead", "HomesteadToEIP150At5")

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

    loadGenesis()
    prepareInitialWorld()

    // TODO sort?
    scenario.blocks.foreach { blockDef =>
      val block = blockDef.rlp.toArray.toBlock
      // TODO handle failure/None
      block.header shouldEqual blockDef.blockHeader.get.toBlockHeader

      val result = ledger.executeBlock(block, storagesInstance.storages, validators)

      result match {
        case Right(receipts) =>
          val actualStateRoot = InMemoryWorldStateProxy.persistState(blockchain.getWorldStateProxy(block.header.number, blockchainConfig.accountStartNonce)).stateRootHash
          val expectedStateRoot = blockDef.blockHeader.get.stateRoot
          actualStateRoot shouldEqual expectedStateRoot

          blockchain.save(block)
          blockchain.save(block.header.hash, receipts)

        case Left(error) =>
          fail(s"Failed to execute block ${blockDef.blocknumber}, reason: $error")
      }
    }

    // TODO more assertions: accounts, header vs rlp, errors, etc.
  }

}



