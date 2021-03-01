package io.iohk.ethereum.ets.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.TxLogEntry
import io.iohk.ethereum.ets.common.TestOptions
import io.iohk.ethereum.network.p2p.messages.PV63.TxLogEntryImplicits._
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.vm.MockWorldState._
import io.iohk.ethereum.vm._
import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration._
import org.scalatest.concurrent.Signaler
import org.scalatest.concurrent.ThreadSignaler
import org.scalatest.concurrent.TimeLimits

class VMSuite extends AnyFreeSpec with Matchers with Logger with TimeLimits {

  val vm = new TestVM

  implicit val signaler: Signaler = ThreadSignaler

  override def run(testName: Option[String], args: Args): Status = {

    val options = TestOptions(args.configMap)
    val scenarios = VMScenarioLoader.load("ets/VMTests", options)

    scenarios.foreach { group =>
      group.name - {
        for {
          (name, scenario) <- group.scenarios
          if options.isScenarioIncluded(name)
        } {
          name in {
            log.info(s"Running test: ${group.name}/$name")
            failAfter(5.minutes) {
              runScenario(scenario)
            }
          }
        }
      }
    }

    runTests(testName, args)
  }

  private def runScenario(scenario: VMScenario): Unit = {
    val context = ScenarioBuilder.prepareContext(scenario)
    val result = deleteAccounts(vm.run(context))
    verifyResult(result, scenario)
  }

  private def verifyResult(result: PR, scenario: VMScenario): Unit = {
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

      val postWorld = ScenarioBuilder.prepareWorld(post, scenario.env.currentNumber, scenario.exec)
      val deadAccounts = postWorld.accounts.keys.filter(postWorld.isAccountDead)
      val expectedWorld = deadAccounts.foldLeft(postWorld)(_ deleteAccount _)
      val actualWorldNoDead = deadAccounts.foldLeft(result.world)(_ deleteAccount _)

      actualWorldNoDead shouldEqual expectedWorld
      deadAccounts.foreach(addr => result.world.isAccountDead(addr) shouldBe true)
    }

    scenario.logs.foreach { expectedLogHash =>
      hashLogs(result.logs) shouldEqual expectedLogHash
    }

    scenario.callcreates.foreach { callcreates =>
      result.internalTxs.flatMap(internalTxToCallCreate) shouldEqual callcreates
    }

    if (
      scenario.gas.isEmpty && scenario.out.isEmpty && scenario.callcreates.isEmpty && scenario.post.isEmpty && scenario.logs.isEmpty
    ) {
      result.error.isDefined shouldBe true
    }
  }

  private def internalTxToCallCreate(itx: InternalTransaction): Option[CallCreate] = {
    if (!Set(TestCREATE, TestCALL, TestCALLCODE).contains(itx.opcode))
      None
    else
      Some(CallCreate(itx.data, itx.to, itx.gasLimit, itx.value))
  }

  private def deleteAccounts(result: PR): PR = {
    val worldAfterDel = result.addressesToDelete.foldLeft(result.world)(_ deleteAccount _)
    result.copy(world = worldAfterDel)
  }

  private def hashLogs(logs: Seq[TxLogEntry]): ByteString = {
    val rlpLogs = RLPList(logs.map(_.toRLPEncodable): _*)
    ByteString(kec256(encode(rlpLogs)))
  }

}
