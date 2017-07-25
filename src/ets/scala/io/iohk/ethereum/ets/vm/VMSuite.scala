package io.iohk.ethereum.ets.vm

import io.iohk.ethereum.domain.TxLogEntry
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.vm.MockWorldState._
import io.iohk.ethereum.vm._
import org.scalatest._

class VMSuite extends FreeSpec with Matchers with Logger {

  override def run(testName: Option[String], args: Args): Status = {

    val options = TestOptions(args.configMap)
    val scenarios = ScenarioLoader.load(options)

    scenarios.foreach { group =>
      group.name - {
        for {
          (name, scenario) <- group.scenarios
          if options.isScenarioIncluded(name)
        } {
          name in {
            log.debug(s"Running test: ${group.name}/$name")
            runScenario(scenario)
          }
        }
      }
    }

    runTests(testName, args)
  }

  private def runScenario(scenario: Scenario): Unit = {
    val context = ScenarioBuilder.prepareContext(scenario)
    val result = VM.run(context)
    verifyResult(result, scenario)
  }

  private def verifyResult(result: PR, scenario: Scenario): Unit = {
    scenario.gas.foreach { gas =>
      result.gasRemaining shouldEqual gas
    }

    scenario.out.foreach { out =>
      result.returnData shouldEqual out
    }

    scenario.post.foreach { post =>
      val expectedWorld = ScenarioBuilder.prepareWorld(post, scenario.env.currentNumber)
      result.world shouldEqual expectedWorld
    }

    scenario.logs.foreach { logs =>
      val expectedLogs = logs.map(l => TxLogEntry(l.address, l.topics, l.data))
      result.logs shouldEqual expectedLogs
    }

    //TODO: callcreate?
  }
}
