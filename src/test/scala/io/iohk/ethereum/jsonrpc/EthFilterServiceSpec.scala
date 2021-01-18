package io.iohk.ethereum.jsonrpc

import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestProbe
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.utils.FilterConfig
import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.jsonrpc.EthFilterService._
import io.iohk.ethereum.jsonrpc.{FilterManager => FM}
import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import scala.concurrent.duration.FiniteDuration
import monix.execution.Scheduler.Implicits.global
import io.iohk.ethereum.jsonrpc.FilterManager.UninstallFilter
import io.iohk.ethereum.jsonrpc.FilterManager.FilterChanges

class EthFilterServiceSpec
    extends TestKit(ActorSystem("EthFilterServiceSpec_ActorSystem"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with OptionValues
    with MockFactory
    with NormalPatience
    with TypeCheckedTripleEquals {

  it should "handle newFilter request" in new TestSetup {
    val filter = Filter(None, None, None, Seq.empty)
    val res = ethFilterService.newFilter(NewFilterRequest(filter)).runToFuture
    filterManager.expectMsg(FM.NewLogFilter(None, None, None, Seq.empty))
    filterManager.reply(FM.NewFilterResponse(123))
    res.futureValue shouldEqual Right(NewFilterResponse(123))
  }

  it should "handle newBlockFilter request" in new TestSetup {
    val res = ethFilterService.newBlockFilter(NewBlockFilterRequest()).runToFuture
    filterManager.expectMsg(FM.NewBlockFilter)
    filterManager.reply(FM.NewFilterResponse(123))
    res.futureValue shouldEqual Right(NewFilterResponse(123))
  }

  it should "handle newPendingTransactionFilter request" in new TestSetup {
    val res = ethFilterService.newPendingTransactionFilter(NewPendingTransactionFilterRequest()).runToFuture
    filterManager.expectMsg(FM.NewPendingTransactionFilter)
    filterManager.reply(FM.NewFilterResponse(123))
    res.futureValue shouldEqual Right(NewFilterResponse((123)))
  }

  it should "handle uninstallFilter request" in new TestSetup {
    val res = ethFilterService.uninstallFilter(UninstallFilterRequest(123)).runToFuture
    filterManager.expectMsg(FM.UninstallFilter(123))
    filterManager.reply(FM.UninstallFilterResponse)
    res.futureValue shouldEqual Right(UninstallFilterResponse(true))
  }

  it should "handle getFilterChanges request" in new TestSetup {
    val res = ethFilterService.getFilterChanges(GetFilterChangesRequest(123)).runToFuture
    filterManager.expectMsg(FM.GetFilterChanges(123))
    val changes = FM.LogFilterChanges(Seq.empty)
    filterManager.reply(changes)
    res.futureValue shouldEqual Right(GetFilterChangesResponse(changes))
  }

  it should "handle getFilterLogs request" in new TestSetup {
    val res = ethFilterService.getFilterLogs(GetFilterLogsRequest(123)).runToFuture
    filterManager.expectMsg(FM.GetFilterLogs(123))
    val logs = FM.LogFilterLogs(Seq.empty)
    filterManager.reply(logs)
    res.futureValue shouldEqual Right(GetFilterLogsResponse(logs))
  }

  it should "handle getLogs request" in new TestSetup {
    val filter = Filter(None, None, None, Seq.empty)
    val res = ethFilterService.getLogs(GetLogsRequest(filter)).runToFuture
    filterManager.expectMsg(FM.GetLogs(None, None, None, Seq.empty))
    val logs = FM.LogFilterLogs(Seq.empty)
    filterManager.reply(logs)
    res.futureValue shouldEqual Right(GetLogsResponse(logs))
  }

  class TestSetup(implicit system: ActorSystem) {
    val filterManager = TestProbe()
    val filterConfig = new FilterConfig {
      override val filterTimeout: FiniteDuration = Timeouts.normalTimeout
      override val filterManagerQueryTimeout: FiniteDuration = Timeouts.normalTimeout
    }

    lazy val ethFilterService = new EthFilterService(
      filterManager.ref,
      filterConfig
    )
  }
}
