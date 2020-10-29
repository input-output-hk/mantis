package io.iohk.ethereum.jsonrpc.server.ipc

import java.io.{BufferedReader, File, InputStreamReader}
import java.net.{ServerSocket, Socket}

import akka.actor.ActorSystem
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers
import io.iohk.ethereum.jsonrpc.server.ipc.JsonRpcIpcServer.JsonRpcIpcServerConfig
import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcRequest}
import io.iohk.ethereum.utils.Logger
import monix.execution.Scheduler.Implicits.global
import org.json4s.JsonAST.JValue
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native
import org.scalasbt.ipcsocket.UnixDomainServerSocket

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Try

class JsonRpcIpcServer(jsonRpcController: JsonRpcController, config: JsonRpcIpcServerConfig)(implicit
    system: ActorSystem
) extends Logger {

  var serverSocket: ServerSocket = _

  def run(): Unit = {
    log.info(s"Starting IPC server: ${config.socketFile}")

    removeSocketFile()

    serverSocket = new UnixDomainServerSocket(config.socketFile)
    new Thread {
      override def run(): Unit = {
        while (!serverSocket.isClosed) {
          val clientSocket = serverSocket.accept()
          // Note: consider using a thread pool to limit the number of connections/requests
          new ClientThread(jsonRpcController, clientSocket).start()
        }
      }
    }.start()
  }

  def close(): Unit = {
    Try(serverSocket.close())
    removeSocketFile()
  }

  private def removeSocketFile(): Unit = {
    val socketFile = new File(config.socketFile)
    if (socketFile.exists()) socketFile.delete()
  }

  class ClientThread(jsonRpcController: JsonRpcController, clientSocket: Socket) extends Thread {

    implicit private val serialization = native.Serialization
    implicit private val formats = JsonSerializers.formats

    private val out = clientSocket.getOutputStream
    private val in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream))

    private val awaitTimeout = 5.minutes

    private var running = true

    override def run(): Unit = {
      while (running) {
        handleNextRequest()
      }
      clientSocket.close()
    }

    @tailrec
    private def readNextMessage(accum: String = ""): Option[JValue] = {
      val buff = new Array[Char](32)
      if (in.read(buff) == -1) {
        None
      } else {
        val newData = new String(buff.takeWhile(c => c != '\n' && c.toByte != 0x0))
        val dataSoFar = accum ++ newData
        parseOpt(dataSoFar) match {
          case Some(json) => Some(json)
          case None => readNextMessage(dataSoFar)
        }
      }
    }

    private def handleNextRequest(): Unit = {
      readNextMessage() match {
        case Some(nextMsgJson) =>
          val request = nextMsgJson.extract[JsonRpcRequest]
          val responseF = jsonRpcController.handleRequest(request)
          val response = responseF.runSyncUnsafe(awaitTimeout)
          out.write((Serialization.write(response) + '\n').getBytes())
          out.flush()
        case None =>
          running = false
      }
    }

  }
}

object JsonRpcIpcServer {
  trait JsonRpcIpcServerConfig {
    val enabled: Boolean
    val socketFile: String
  }

  object JsonRpcIpcServerConfig {
    import com.typesafe.config.{Config => TypesafeConfig}

    def apply(mantisConfig: TypesafeConfig): JsonRpcIpcServerConfig = {
      val rpcIpcConfig = mantisConfig.getConfig("network.rpc.ipc")

      new JsonRpcIpcServerConfig {
        override val enabled: Boolean = rpcIpcConfig.getBoolean("enabled")
        override val socketFile: String = rpcIpcConfig.getString("socket-file")
      }
    }
  }
}
