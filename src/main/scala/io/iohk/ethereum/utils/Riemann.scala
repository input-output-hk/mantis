package io.iohk.ethereum.utils

import io.riemann.riemann.client.RiemannClient
import io.riemann.riemann.client.Transport
import io.riemann.riemann.client.EventDSL
import io.riemann.riemann.client.IRiemannClient
import io.riemann.riemann.client.IPromise
import io.riemann.riemann.client.Promise
import io.riemann.riemann.client.ChainPromise
import io.riemann.riemann.Proto.Event
import io.riemann.riemann.Proto.Msg
import java.net.InetAddress
import java.util.concurrent.TimeUnit
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.BlockingQueue
import java.util.LinkedList
import java.io.IOException
import scala.collection.JavaConverters._
import java.util.concurrent.Executors
import concurrent.{ExecutionContext, Future}
import com.googlecode.protobuf.format.FormatFactory

trait Riemann extends Logger {

  private val hostName = Config.Network.riemann
    .map { c =>
      c.hostName
    }
    .getOrElse(InetAddress.getLocalHost().getHostName())

  private val riemannClient = {
    log.debug("create new RiemannClient")
    val c = {
      Config.Network.riemann match {
        case Some(config) => {
          log.debug(
            s"create new riemann batch client connecting to ${config.host}:${config.port}")
          val client = new RiemannBatchClient(config)
          try {
            client.connect()
            client
          } catch {
            case e: IOException =>
              log.error(e.toString)
              log.error(
                "failed to create riemann batch client, falling back to stdout client")
              new RiemannStdoutClient()
          }
        }
        case None => {
          log.debug("create new stdout riemann client")
          new RiemannStdoutClient()
        }
      }
    }
    c.connect()
    log.debug("riemann client connected")
    c
  }

  def close: Unit = riemannClient.close()

  def defaultEvent: EventDSL = {
    val event = new EventDSL(riemannClient)
    val microsenconds =
      TimeUnit.MILLISECONDS.toMicros(System.currentTimeMillis());
    event.time(microsenconds).host(hostName)
  }

  def ok(service: String): EventDSL = defaultEvent.state("ok").service(service)
  def warning(service: String): EventDSL = defaultEvent.state("warning").service(service)
  def error(service: String): EventDSL = defaultEvent.state("error").service(service)
  def exception(service: String, t: Throwable): EventDSL = {
    // Format message and stacktrace
    val desc = new StringBuilder();
    desc.append(t.toString())
    desc.append("\n\n");
    t.getStackTrace.map { e =>
      desc.append(e)
      desc.append("\n")
    }
    defaultEvent
      .service(service)
      .state("error")
      .tag("exception")
      .tag(t.getClass().getSimpleName())
      .description(desc.toString())
  }
  def critical(service: String): EventDSL = defaultEvent.state("critical").service(service)

}

object Riemann extends Riemann with Logger {}

class RiemannBatchClient(config: RiemannConfiguration)
    extends IRiemannClient
    with Logger {
  private def promise[A](v: A): Promise[A] = {
    val p: Promise[A] = new Promise()
    p.deliver(v)
    p
  }
  private def simpleMsg(event: Event) = {
    val msg = Msg.newBuilder().addEvents(event).build()
    promise(msg)
  }
  private val jsonFormatter =
    (new FormatFactory()).createFormatter(FormatFactory.Formatter.JSON)

  protected val queue: BlockingQueue[Event] = new ArrayBlockingQueue(
    config.bufferSize)

  private val client = RiemannClient.tcp(config.host, config.port)

  protected def sendBatch: Unit = {
    val batch: LinkedList[Event] = new LinkedList()
    queue.drainTo(batch, config.batchSize)
    try {
      log.trace("try to send batch")
      val p = client.sendEvents(batch)
      client.flush()
      val result = p.deref()
      log.trace(s"sent batch with result: $result")
    } catch {
      case e: IOException =>
        log.error(e.toString)
        batch.asScala
          .map { event =>
            {
              System.err.println(s"${jsonFormatter.printToString(event)}")
            }
          }
    }
  }

  implicit val context =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(20))

  val f = Future {
    while (true) {
      if (queue.size() > 0) {
        log.trace("sending batch of Riemann events")
        sendBatch
        log.trace("sent batch of Riemann events")
      } else {
        log.trace("wait for next batch of Riemann events")
        Thread.sleep(5000)
      }
    }
  }

  val flusher = Future {
    while (true) {
      Thread.sleep(config.autoFlushMilliseconds)
      log.trace("auto flush riemann queue")
      client.flush()
    }
  }

  override def connect() = client.connect()
  override def sendEvent(event: Event) = {
    val res = queue.offer(event)
    if (!res) {
      log.error("Riemann buffer full")
      System.err.println(s"${jsonFormatter.printToString(event)}")
    }
    simpleMsg(event)
  }
  override def sendMessage(msg: Msg): IPromise[Msg] = client.sendMessage(msg)

  override def event(): EventDSL = {
    new EventDSL(this)
  }
  override def query(q: String): IPromise[java.util.List[Event]] =
    client.query(q)
  override def sendEvents(events: java.util.List[Event]): IPromise[Msg] = {
    val p = new ChainPromise[Msg]
    events.asScala.map { e =>
      val clientPromise = sendEvent(e)
      p.attach(clientPromise)
    }
    p
  }
  override def sendEvents(events: Event*): IPromise[Msg] = {
    val p = new ChainPromise[Msg]
    events.map { e =>
      val clientPromise = sendEvent(e)
      p.attach(clientPromise)
    }
    p
  }
  override def sendException(service: String, t: Throwable): IPromise[Msg] =
    client.sendException(service, t)
  override def close(): Unit = client.close()
  override def flush(): Unit = client.flush()
  override def isConnected(): Boolean = client.isConnected
  override def reconnect(): Unit = client.reconnect()
  override def transport(): Transport = this
}

class RiemannStdoutClient extends IRiemannClient {
  private var connected = false
  private def promise[A](v: A): Promise[A] = {
    val p: Promise[A] = new Promise()
    p.deliver(v)
    p
  }
  private def simpleMsg(event: Event) = {
    val msg = Msg.newBuilder().addEvents(event).build()
    promise(msg)
  }
  private val jsonFormatter =
    (new FormatFactory()).createFormatter(FormatFactory.Formatter.JSON)
  override def connect() = {
    connected = true
  }
  override def sendEvent(event: Event) = {
    println(s"${jsonFormatter.printToString(event)}")
    simpleMsg(event)
  }
  override def sendMessage(msg: Msg): IPromise[Msg] = {
    println(s"${jsonFormatter.printToString(msg)}")
    promise(msg)
  }
  override def event(): EventDSL = {
    new EventDSL(this)
  }
  override def query(q: String): IPromise[java.util.List[Event]] = {
    promise(new java.util.LinkedList())
  }
  override def sendEvents(events: java.util.List[Event]): IPromise[Msg] = {
    events.asScala.map { e =>
      println(s"${jsonFormatter.printToString(e)}")
    }
    val msg = Msg.newBuilder().build()
    promise(msg)
  }
  override def sendEvents(events: Event*): IPromise[Msg] = {
    events.map { e =>
      println(s"${jsonFormatter.printToString(e)}")
    }
    val msg = Msg.newBuilder().build()
    promise(msg)
  }
  override def sendException(service: String, t: Throwable): IPromise[Msg] = {
    System.err.println(s"service: ${service}\n${t}")
    val msg = Msg.newBuilder().build()
    promise(msg)
  }
  override def close(): Unit = {
    connected = false
  }
  override def flush(): Unit = {}
  override def isConnected(): Boolean = connected
  override def reconnect(): Unit = {
    connected = true
  }
  override def transport(): Transport = this

}

trait ToRiemann {
  def toRiemann: EventDSL
}
