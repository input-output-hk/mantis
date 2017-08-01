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
    val result = deleteAccounts(VM.run(context))
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
      // We assert on dead accounts separately (see EIP-161 for definition of dead).
      // Each empty account specified in the post section must be dead in the resulting world
      // (non-dead accounts must be the same in post section and the actual resulting world).
      // The reason for this is that certain opcodes like EXTCODECOPY may create a new empty account
      // if the address they target does not exist. This is not normal behaviour for the actual VM and such accounts
      // are not created on the main chain, even pre EIP-161. It is rather an implementation artifact of the test VM
      // implemented in cpp-ethereum (and duplicated in the likes of Parity).
      // See: https://github.com/ethereum/cpp-ethereum/issues/4281

      val postWorld = ScenarioBuilder.prepareWorld(post, scenario.env.currentNumber)
      val deadAccounts = postWorld.accounts.keys.filter(postWorld.isAccountDead)
      val expectedWorld = deadAccounts.foldLeft(postWorld)(_ deleteAccount _)
      val actualWorldNoDead = deadAccounts.foldLeft(result.world)(_ deleteAccount _)

      actualWorldNoDead shouldEqual expectedWorld
      deadAccounts.foreach(addr => result.world.isAccountDead(addr) shouldBe true)
    }

    scenario.logs.foreach { logs =>
      val expectedLogs = logs.map(l => TxLogEntry(l.address, l.topics, l.data))
      result.logs shouldEqual expectedLogs
    }

    scenario.callcreates.foreach { callcreates =>
      result.internalTxs.flatMap(internalTxToCallCreate) shouldEqual callcreates
    }

    //TODO: when no expectations, expect error
  }

  private def internalTxToCallCreate(itx: InternalTransaction): Option[CallCreate] = {
    if (!Set(TestCREATE, TestCALL).contains(itx.opcode))
      None
    else
      Some(CallCreate(itx.data, itx.to, itx.gasLimit, itx.value))
  }

  private def deleteAccounts(result: PR): PR = {
    val worldAfterDel = result.addressesToDelete.foldLeft(result.world)(_ deleteAccount _)
    result.copy(world = worldAfterDel)
  }

}
