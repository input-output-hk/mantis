package io.iohk.ethereum.metrics

import java.util.concurrent.TimeUnit

import io.iohk.ethereum.utils.events.{EventAttr, EventState, EventSupport, EventTag, EventsDSL}
import io.micrometer.core.instrument._
import io.micrometer.core.instrument.step.StepMeterRegistry
import io.micrometer.core.instrument.util.MeterPartition
import io.riemann.riemann.Proto
import io.riemann.riemann.client.IRiemannClient

import scala.collection.JavaConverters._

/**
 * A registry that publishes events to Riemann.
 */
class RiemannRegistry(
  config: RiemannRegistryConfig,
  percentiles: Percentiles,
  clientF: () ⇒ IRiemannClient
) extends StepMeterRegistry(config, Metrics.StdMetricsClock) with EventSupport {

  protected def getBaseTimeUnit: TimeUnit = TimeUnit.MILLISECONDS

  def mainService: String = getClass.getSimpleName

  private def newEvent(buffer: EventsDSL, m: Meter, suffix: String): EventsDSL = {
    require(!suffix.startsWith("."), "suffix does not start with '.'")

    val name = m.getId.getName
    val service = if(suffix.trim.isEmpty) name else name + "." + suffix

    buffer.newEvent()
      .currentTime()
      .host(config.hostForEvents)
      .service(service)
      .tag(EventTag.Metric)
      .state(EventState.OK)
      .attribute(EventAttr.Type, m.getId.getType.name())
      .attribute(EventAttr.Unit, m.getId.getBaseUnit)
  }

  private def newEvent(buffer: EventsDSL, m: Meter): EventsDSL = newEvent(buffer, m, "")

  private def eventsForTimer(t: Timer): Seq[Proto.Event] = {
    val baseTimeUnit = t.baseTimeUnit()
    val buffer = EventsDSL()

    newEvent(buffer, t, "totalTime").metric(t.totalTime(baseTimeUnit))
    newEvent(buffer, t, "mean").metric(t.mean(baseTimeUnit))
    newEvent(buffer, t, "max").metric(t.max(baseTimeUnit))
    newEvent(buffer, t, "count").metric(t.count())

    val snapshot = t.takeSnapshot()
    val pavs = snapshot.percentileValues()

    for {
      pav ← pavs
      Percentile(_, name) ← percentiles.byNumber.get(pav.percentile())
    } {
      newEvent(buffer, t, name).metric(pav.value(baseTimeUnit))
    }

    val events = buffer.build().getEventsList.asScala

    events
  }

  private def eventsForFunctionTimer(t: FunctionTimer): Seq[Proto.Event] = {
    val baseTimeUnit = t.baseTimeUnit()
    val buffer = EventsDSL()

    newEvent(buffer, t, "totalTime").metric(t.totalTime(baseTimeUnit))
    newEvent(buffer, t, "mean").metric(t.mean(baseTimeUnit))
    newEvent(buffer, t, "count").metric(t.count())

    val events = buffer.build().getEventsList.asScala

    events
  }

  private def eventsForLongTaskTimer(t: LongTaskTimer): Seq[Proto.Event] = {
    val buffer = EventsDSL()

    newEvent(buffer, t, "activeTasks").metric(t.activeTasks())
    // TODO make sure the time unit from `t.getId.getBaseUnit` is `TimeUnit.NANOSECONDS`
    newEvent(buffer, t, "duration").metric(t.duration(TimeUnit.NANOSECONDS))

    val events = buffer.build().getEventsList.asScala

    events
  }

  private def eventsForDistributionSummary(s: DistributionSummary): Seq[Proto.Event] = {
    val buffer = EventsDSL()

    newEvent(buffer, s, "totalAmount").metric(s.totalAmount())
    newEvent(buffer, s, "mean").metric(s.mean())
    newEvent(buffer, s, "max").metric(s.max())
    newEvent(buffer, s, "count").metric(s.count())

    val snapshot = s.takeSnapshot()
    val pavs = snapshot.percentileValues()

    for {
      pav ← pavs
      Percentile(_, name) ← percentiles.byNumber.get(pav.percentile())
    } {
      newEvent(buffer, s, name).metric(pav.value())
    }

    val events = buffer.build().getEventsList.asScala

    events
  }

  private def eventsForGauge(g: Gauge): Seq[Proto.Event] = {
    val buffer = EventsDSL()

    newEvent(buffer, g).metric(g.value())

    val events = buffer.build().getEventsList.asScala

    events
  }

  private def eventsForTimeGauge(g: TimeGauge): Seq[Proto.Event] = {
    val buffer = EventsDSL()

    newEvent(buffer, g).metric(g.value(g.baseTimeUnit()))

    val events = buffer.build().getEventsList.asScala

    events
  }

  private def eventsForCounter(c: Counter): Seq[Proto.Event] = {
    val buffer = EventsDSL()

    newEvent(buffer, c).metric(c.count())

    val events = buffer.build().getEventsList.asScala

    events
  }


  private def eventsForFunctionCounter(c: FunctionCounter): Seq[Proto.Event] = {
    val buffer = EventsDSL()

    newEvent(buffer, c).metric(c.count())

    val events = buffer.build().getEventsList.asScala

    events
  }

  private def eventsForMeter(m: Meter): Seq[Proto.Event] = {
    val buffer = EventsDSL()

    newEvent(buffer, m).description(m.toString).state(EventState.Error).tag(EventTag.Unhandled)

    val events = buffer.build().getEventsList.asScala

    events
  }

  def generateEvents(meter: Meter): Seq[Proto.Event] =
    meter match {
      case s: DistributionSummary ⇒ eventsForDistributionSummary(s)

      case t: FunctionTimer ⇒ eventsForFunctionTimer(t)
      case t: LongTaskTimer ⇒ eventsForLongTaskTimer(t)

      case t: Timer ⇒ eventsForTimer(t)

      case g: TimeGauge ⇒ eventsForTimeGauge(g)
      case g: Gauge ⇒ eventsForGauge(g)

      case c: FunctionCounter ⇒ eventsForFunctionCounter(c)
      case c: Counter ⇒ eventsForCounter(c)

      // catch-all
      case m: Meter ⇒ eventsForMeter(m)
    }

  def publish(): Unit = {
    Event.okStart().send()
    val partitionsJ = MeterPartition.partition(this, config.batchSize())
    val partitions = partitionsJ.asScala.map(_.asScala)

    val events = try {
      for {
        meters ← partitions
        _ = Event.ok("meters").metric(meters.size).send()
        meter ← meters
        events = generateEvents(meter)
        event ← events
      } yield {
        event
      }
    }
    catch {
      case t: Throwable ⇒
        t.printStackTrace(System.err)
        Event.exception(t).send()
        List[Proto.Event]()
    }

    try {
      clientF().sendEvents(events.asJava)
      Event.okFinish().metric(events.size).send()
    }
    catch {
      case t: Throwable ⇒
        t.printStackTrace(System.err)
        Event.exception(t).send()
    }
  }
}
