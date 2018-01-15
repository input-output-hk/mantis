package io.iohk.ethereum

import java.io.File
import java.net.URLClassLoader

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Framing, Sink, Source, SourceQueueWithComplete, Tcp}
import akka.util.ByteString

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

// scalastyle:off

// assumes a single TCP client for now
object ExternalVm extends {

  implicit val system = ActorSystem("EVM_System")
  implicit val materializer = ActorMaterializer()

  val executeRe = "Execute\\(([0-9]+)\\,([0-9]+)\\)".r

  var outMessagesQueue: SourceQueueWithComplete[String] = _

  def main(args: Array[String]): Unit = {
    Tcp().bind(args(0), args(1).toInt)
      .runForeach(connection => handleConnection(connection.flow))
  }

  def handleConnection(connection: Flow[ByteString, ByteString, NotUsed]) = {
    outMessagesQueue = Source.queue[String](1024, OverflowStrategy.dropHead)
      .map(s => ByteString(s.getBytes))
      .via(connection)
      .via(Framing.delimiter(
        ByteString("\n"),
        maximumFrameLength = 1024,
        allowTruncation = true))
      .map(_.utf8String)
      .to(Sink.foreach(handleMessageFromClient))
      .run()
  }

  private def sendMessageToClient(msg: String): Unit = {
    outMessagesQueue offer msg
  }

  private def handleMessageFromClient(msg: String): Unit = {
    msg match {
      case executeRe(rawExecutionId, rawIntParam) =>
        val num = rawIntParam.toInt
        val result = num * 2
        sendMessageToClient(s"Result($rawExecutionId,$result)\n")
    }
  }

}

class ExternalVmProxy(host: String, port: Int)(implicit actorSystem: ActorSystem) {

  type ExecutionId = Long
  case class Execution(executionId: ExecutionId, num: Int, promise: Promise[ExecutionResult])
  case class ExecutionResult(executionId: ExecutionId, num: Int, result: Int)

  val resultRe = "Result\\(([0-9]+)\\,([0-9]+)\\)".r

  implicit val materializer = ActorMaterializer()

  var pendingExecutions: Map[ExecutionId, Execution] = Map.empty

  val connection = Tcp().outgoingConnection(host, port)

  val outMessagesQueue =
    Source.queue[String](1024, OverflowStrategy.dropHead)
      .map(s => ByteString(s.getBytes))
      .via(connection)
      .via(Framing.delimiter(
        ByteString("\n"),
        maximumFrameLength = 1024,
        allowTruncation = true))
      .map(_.utf8String)
      .to(Sink.foreach(handleMessageFromVM))
      .run()

  def execute(num: Int): Future[ExecutionResult] = {
    val promise = Promise[ExecutionResult]()
    val execution = Execution(0, num, promise)
    pendingExecutions += (execution.executionId -> execution)
    sendMessageToVM(s"Execute(${execution.executionId},$num)\n")
    promise.future
  }

  private def sendMessageToVM(msg: String): Unit = {
    outMessagesQueue offer msg
  }

  private def handleMessageFromVM(msg: String): Unit = {
    msg match {
      case resultRe(rawExecutionId, rawResult) =>
        val executionId = rawExecutionId.toInt
        pendingExecutions.get(executionId).foreach { execution =>
          execution.promise.trySuccess(ExecutionResult(execution.executionId, execution.num, rawResult.toInt))
          pendingExecutions -= executionId
        }

      case _ =>
        println("received query for additional data from vm")
    }
  }

}

object RemoteVmTest {

  def startVMInThisProcess(vmHost: String, vmPort: Int): Unit = {
    ExternalVm.main(Array(vmHost, vmPort.toString))
  }

  def startVMProcess(vmHost: String, vmPort: Int): Unit = {
    val classpath = Thread.currentThread().getContextClassLoader.asInstanceOf[URLClassLoader].getURLs
      .map(_.getFile)
      .mkString(File.pathSeparator)

    new ProcessBuilder(
      System.getProperty("java.home") + "/bin/java",
      "-classpath",
      classpath,
      "io.iohk.ethereum.ExternalVm",
      vmHost,
      vmPort.toString
    )
      .inheritIO()
      .start()
  }

  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem("RemoteVmTest_System")

    val vmHost = "127.0.0.1"
    val vmPort = 8888

    if (Thread.currentThread().getContextClassLoader.isInstanceOf[URLClassLoader]) {
      startVMProcess(vmHost, vmPort)
    } else {
      startVMInThisProcess(vmHost, vmPort)
    }

    Thread.sleep(1000)
    val externalVmProxy = new ExternalVmProxy(vmHost, vmPort)
    val resultFuture = externalVmProxy.execute(100)
    resultFuture.foreach  { res =>
      println("Got result from vm = " + res)
    }

  }

}
