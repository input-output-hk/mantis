package io.iohk.ethereum.utils.events

import io.iohk.ethereum.utils.Riemann

trait EventSupport {
  // The main service is always a prefix of the event service.
  // Use something short.
  protected def mainService: String

  // An implementor decides what else is added in the event.
  protected def postProcessEvent(event: EventDSL): EventDSL = event

  private[this] def mkService(moreService: String): String =
    if(moreService.isEmpty) mainService else s"${mainService} ${moreService}"

  private[this] def postProcessAndTagMainService(event: EventDSL): EventDSL =
    postProcessEvent(event).tag(mainService)

  // DSL convenience for the eye.
  @inline final protected def Event: this.type = this

  // DSL convenience for the eye.
  // Use like this:
  //    Event(event).send()
  // when `event` comes from elsewhere (e.g. a `ToRiemann.toRiemann` call)
  @inline final protected def apply(event: EventDSL): event.type = event

  protected def ok(moreService: String): EventDSL = {
    val service = mkService(moreService)
    val event = Riemann.ok(service)
    postProcessAndTagMainService(event)
  }

  protected def ok(): EventDSL = ok("")

  protected def okStart(): EventDSL = ok(EventTag.Start).tag(EventTag.Start)
  protected def okFinish(): EventDSL = ok(EventTag.Finish).tag(EventTag.Finish)

  protected def warning(moreService: String): EventDSL = {
    val service = mkService(moreService)
    val event = Riemann.warning(service)
    postProcessAndTagMainService(event)
  }

  protected def warningStart(): EventDSL = warning(EventTag.Start).tag(EventTag.Start)

  protected def error(moreService: String): EventDSL = {
    val service = mkService(moreService)
    val event = Riemann.warning(service)
    postProcessAndTagMainService(event)
  }

  protected def exception(moreService: String, t: Throwable): EventDSL = {
    val service = mkService(moreService)
    val event = Riemann.exception(service, t)
    postProcessAndTagMainService(event)
  }

  protected def exception(t: Throwable): EventDSL = exception("", t)
  protected def exceptionFinish(t: Throwable): EventDSL = exception(EventTag.Finish, t).tag(EventTag.Finish)
}
