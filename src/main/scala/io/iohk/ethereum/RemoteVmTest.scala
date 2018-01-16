package io.iohk.ethereum

// scalastyle:off

import java.io.File
import java.net.URLClassLoader

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Framing, Sink, Source, SourceQueueWithComplete, Tcp}
import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.domain.{Account, Address, BlockHeader, UInt256}

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.mpt.NodesKeyValueStorage
import io.iohk.ethereum.network.p2p.{EthereumMessageDecoder, Message}
import io.iohk.ethereum.network.p2p.messages.VmMessages.{AccountResponse, BlockHashResponse, CodeResponse, Context, Execute, GetAccount, GetBlockHash, GetCode, GetStorageData, StorageData}
import io.iohk.ethereum.network.rlpx.{MessageCodec, VmFrameCodec}
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.vm.{WorldStateProxy, _}
import org.spongycastle.util.encoders.Hex

import scala.collection.mutable


// assumes a single TCP client for now
object ExternalVm extends {

  val messageCodec = new MessageCodec(new VmFrameCodec, EthereumMessageDecoder, 4)

  implicit val system = ActorSystem("EVM_System")
  implicit val materializer = ActorMaterializer()

  var outMessagesQueue: SourceQueueWithComplete[ByteString] = _

  def main(args: Array[String]): Unit = {
    Tcp().bind(args(0), args(1).toInt)
      .runForeach(connection => handleConnection(connection.flow))
  }

  def handleConnection(connection: Flow[ByteString, ByteString, NotUsed]) = {
    outMessagesQueue = Source.queue[ByteString](1024, OverflowStrategy.dropHead)
      .via(connection)
      .to(Sink.foreach(handleDataFromClient))
      .run()
  }

  private def handleDataFromClient(byteString: ByteString): Unit = {
    val messages = messageCodec.readMessages(byteString)
    messages.map(_.get).foreach {
      case execute: Execute =>
        val runResult = run(execute)
        println("run result=" + runResult)

      case other =>
        println("Vm received unexpected msg: " + other)
    }
  }

  private def run(execute: Execute) = {
    val context = ProgramContext[World, Storage](
      env = execute.context.env,
      receivingAddr = execute.context.receivingAddr,
      startGas = execute.context.startGas,
      world = new World(0, 0, true),
      config = EvmConfig.forBlock(0, BlockchainConfig(Config.config)),
      initialAddressesToDelete = Set.empty[Address])

    VM.run(context)
  }

  class Storage(executionId: Long, address: Address) extends vm.Storage[Storage] {
    val storage = mutable.Map[UInt256, UInt256]()

    def store(offset: UInt256, value: UInt256): Storage = {
      storage += offset -> value
      this
    }

    def load(offset: UInt256): UInt256 = storage.get(offset) match {
      case Some(value) =>
        value

      case None =>
        val getStorageDataMsg = GetStorageData(executionId, address = address, offset = offset)

        outMessagesQueue offer messageCodec.encodeMessage(getStorageDataMsg)

        val storageData = awaitMessage[StorageData]()

        val value: UInt256 = storageData.data

        storage += offset -> value
        value
    }
  }

  private def awaitMessage[M <: Message](): M = {
    ???
  }

  class World(executionId: Long,
               accountStartNonce: UInt256,
               noEmptyAccountsCond: Boolean
             ) extends vm.WorldStateProxy[World, Storage] {
    val accounts = mutable.Map[Address, Option[Account]]()
    val modifiedAccounts = mutable.Set[Address]()
    val storages = mutable.Map[Address, Storage]()
    val codeRepo = mutable.Map[Address, ByteString]()
    val blockHashes = mutable.Map[UInt256, Option[UInt256]]()
    val touchedAccounts = mutable.Set.empty[Address]

    def getAccount(address: Address): Option[Account] = accounts.getOrElse(address, {

      outMessagesQueue offer messageCodec.encodeMessage(GetAccount(executionId, address))

      val accountResponse = awaitMessage[AccountResponse]()
      val account = accountResponse.account // TODO: option!?!?!?

      /*
      if (account.nonce.isEmpty) {
        accounts += address -> None
        None
      } else {
        val account = Account(account.nonce, account.balance, account.storage)
        accounts += address -> Some(account)
        Some(account)
      }
    */
      accounts += address -> Some(account)
      Some(account)
    })

    def saveAccount(address: Address, account: Account): World = {
      modifiedAccounts += address
      accounts += address -> Some(account)
      this
    }

    protected def deleteAccount(address: Address): World = {
      accounts += address -> None
      this
    }

    protected def getEmptyAccount: Account = Account.empty(accountStartNonce)

    def touchAccounts(addresses: Address*): World = {
      touchedAccounts ++= addresses
      this
    }

    protected def clearTouchedAccounts: World = {
      touchedAccounts.clear()
      this
    }

    protected def noEmptyAccounts: Boolean = noEmptyAccountsCond

    def combineTouchedAccounts(world: World): World = {
      touchedAccounts ++= world.touchedAccounts
      this
    }

    def getCode(address: Address): ByteString = codeRepo.getOrElse(address, {
      outMessagesQueue offer messageCodec.encodeMessage(GetCode(executionId, address))

      val codeResponse = awaitMessage[CodeResponse]()

      val code: ByteString = codeResponse.data
      codeRepo += address -> code
      code
    })

    def getStorage(address: Address): Storage = storages.getOrElse(address, {
      val storage = new Storage(executionId, address)
      storages += address -> storage
      storage
    })

    def getBlockHash(number: UInt256): Option[UInt256] = blockHashes.getOrElse(number, {
      outMessagesQueue offer messageCodec.encodeMessage(GetBlockHash(executionId, if (number > Int.MaxValue) - 1 else number.toInt))

      val blockHashResponse = awaitMessage[BlockHashResponse]()

      if (blockHashResponse.hash.isEmpty) {
        blockHashes += number -> None
        None
      } else {
        val hash: UInt256 = UInt256(blockHashResponse.hash)
        blockHashes += number -> Some(hash)
        Some(hash)
      }
    })

    def saveCode(address: Address, code: ByteString): World = {
      modifiedAccounts += address
      codeRepo += address -> code
      this
    }

    def saveStorage(address: Address, storage: Storage): World = {
      modifiedAccounts += address
      storages += address -> storage
      this
    }
  }

}

class ExternalVmProxy(host: String, port: Int)(implicit actorSystem: ActorSystem) {

  val messageCodec = new MessageCodec(new VmFrameCodec, EthereumMessageDecoder, 4)

  type ExecutionId = Long
  case class Execution(executionId: ExecutionId, num: Int, promise: Promise[ExecutionResult])
  case class ExecutionResult(executionId: ExecutionId, num: Int, result: Int)

  implicit val materializer = ActorMaterializer()

  var pendingExecutions: Map[ExecutionId, Execution] = Map.empty

  val connection = Tcp().outgoingConnection(host, port)

  val outMessagesQueue =
    Source.queue[ByteString](1024, OverflowStrategy.dropHead)
      .via(connection)
      .to(Sink.foreach(handleMessageFromVM))
      .run()

  override def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
    val out = messageCodec.encodeMessage(Execute(123, Context(context.env, context.receivingAddr, context.startGas)))
    outMessagesQueue offer out

    promise.future
  }

  private def handleMessageFromVM(byteString: ByteString): Unit = {
    val messages = messageCodec.readMessages(byteString)
    println("got messages from vm: " + messages)
/*
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
    */
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
