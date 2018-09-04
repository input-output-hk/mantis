package io.iohk.ethereum.utils.events

import java.util.concurrent.TimeUnit

import io.riemann.riemann.Proto.Msg
import io.riemann.riemann.client.EventBuilder

/**
 * This is like [[io.riemann.riemann.client.EventDSL EventDSL]] but
 * a) for more than one events, and
 * b) without requiring an [[io.riemann.riemann.client.IRiemannClient IRiemannClient]].
 *
 * @see [[io.iohk.ethereum.metrics.RiemannRegistry]]
 */
final class EventsDSL {
  private[this] val allEvents: Msg.Builder = Msg.newBuilder()
  private[this] var workingEvent: EventBuilder = _

  private[this] def addWorkingEvent(): Unit = {
    if(workingEvent ne null) allEvents.addEvents(workingEvent.build())
  }

  def newEvent(): EventsDSL = {
    addWorkingEvent()
    workingEvent = new EventBuilder()
    this
  }

  def build(): Msg = {
    addWorkingEvent()
    allEvents.build()
  }

  def host(host: String): EventsDSL = {
    workingEvent.host(host)
    this
  }

  def service(service: String): EventsDSL = {
    workingEvent.service(service)
    this
  }

  def state(state: String): EventsDSL = {
    workingEvent.state(state)
    this
  }

  def description(description: String): EventsDSL = {
    workingEvent.description(description)
    this
  }

  def time(time: Double): EventsDSL = {
    workingEvent.time(time)
    this
  }

  def time(time: Long): EventsDSL = {
    workingEvent.time(time)
    this
  }

  def currentTime(): EventsDSL = {
    workingEvent.time(TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis()))
    this
  }

  def metric(metric: Int): EventsDSL = {
    workingEvent.metric(metric)
    this
  }

  def metric(metric: Long): EventsDSL = {
    workingEvent.metric(metric)
    this
  }

  def metric(metric: Double): EventsDSL = {
    workingEvent.metric(metric)
    this
  }

  def tag(tag: String): EventsDSL = {
    if(tag ne null) {
      workingEvent.tag(tag)
    }
    this
  }

  def attribute(name: String, value: String): EventsDSL = {
    if(value ne null) {
      // Note if we set a null value, there will be an exception
      // from the generated protobuf builder.
      workingEvent.attribute(name, value)
    }
    this
  }
}

object EventsDSL {
  def apply(): EventsDSL = new EventsDSL
}
