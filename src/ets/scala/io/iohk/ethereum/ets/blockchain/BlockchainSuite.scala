package io.iohk.ethereum.ets.blockchain

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.ets.common.{ScenarioLoader, TestOptions}
import io.iohk.ethereum.rlp
import io.iohk.ethereum.utils.Logger
import org.scalatest._


class BlockchainSuite extends FreeSpec with Matchers with Logger {

  val unsupportedNetworks = Set("Byzantium","Constantinople")

  override def run(testName: Option[String], args: Args): Status = {

    val options = TestOptions(args.configMap)
    val scenarios = ScenarioLoader.load[Scenario]("ets/BlockchainTests", options)

    scenarios.foreach { group =>
      group.name - {
        for {
          (name, scenario) <- group.scenarios
          if options.isScenarioIncluded(name)
        } {
          name in new ScenarioSetup(scenario) {
            log.debug(s"Running test: ${group.name}/$name")
            runScenario(scenario, this)
          }
        }
      }
    }

    runTests(testName, args)
  }


  private def runScenario(scenario: Scenario, setup: ScenarioSetup): Unit = {
    import setup._

    val genesisHeader = loadGenesis()
    genesisHeader shouldEqual scenario.genesisBlockHeader.toBlockHeader

    prepareInitialWorld()

    // TODO sort?
    scenario.blocks.foreach { blockDef =>
      val block = rlp.decode[Block](scenario.genesisRLP.toArray)
      // TODO handle failure/None
      block.header shouldEqual blockDef.blockHeader.get

      val result = ledger.executeBlock(block, storagesInstance.storages, validators)

      result match {
        case Right(receipts) =>
          blockchain.save(block)
          blockchain.save(block.header.hash, receipts)

        case Left(error) =>
          fail(s"Failed to execute block ${blockDef.blocknumber}, reason: $error")
      }
    }

    val lastBlockNumber = scenario.blocks.lastOption.flatMap(_.blockHeader).map(_.number)
    val lastBlockStateRoot = lastBlockNumber.map(n => blockchain.getBlockByNumber(n).get) // TODO handle failure
    lastBlockStateRoot.foreach(_ shouldEqual expectedStateRoot)
  }

}



