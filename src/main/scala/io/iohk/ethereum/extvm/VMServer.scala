package io.iohk.ethereum.extvm

import java.nio.ByteOrder

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Framing, Keep, Sink, SinkQueueWithCancel, Source, SourceQueueWithComplete, Tcp}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.util.ByteString
import com.google.protobuf.{ByteString => GByteString}
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.extvm.Implicits._
import io.iohk.ethereum.utils._
import io.iohk.ethereum.vm
import io.iohk.ethereum.vm.EvmConfig.BlockchainConfigForEvm
import io.iohk.ethereum.vm.ProgramResult

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object VmServerApp {

  implicit val system = ActorSystem("EVM_System")
  implicit val materializer = ActorMaterializer()

  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load()
    Tcp().bind(config.getString("mantis.extvm.host"), config.getInt("mantis.extvm.port"))
      .runForeach(connection => handleConnection(connection.flow))
  }

  def handleConnection(connection: Flow[ByteString, ByteString, NotUsed]): Unit = {
    val (out, in) = Source.queue[ByteString](QueueBufferSize, OverflowStrategy.dropTail)
      .via(connection)
      .via(Framing.lengthField(LengthPrefixSize, 0, Int.MaxValue, ByteOrder.BIG_ENDIAN))
      .map(_.drop(LengthPrefixSize))
      .toMat(Sink.queue[ByteString]())(Keep.both)
      .run()

    new VMServer(in, out).run()
  }
}

class VMServer(
    in: SinkQueueWithCancel[ByteString],
    out: SourceQueueWithComplete[ByteString])
  extends Logger {

  private val messageHandler = new MessageHandler(in, out)

  @tailrec
  private def processNextCall(): Unit = {
    Try {
      val callContext = messageHandler.awaitMessage[msg.CallContext]

      log.debug("Server received msg: CallContext")

      val context = constructContextFromMsg(callContext)
      val result = vm.VM.run(context)

      val callResultMsg = buildResultMsg(result)
      val queryMsg = msg.VMQuery(query = msg.VMQuery.Query.CallResult(callResultMsg))
      messageHandler.sendMessage(queryMsg)
    } match {
      case Success(_) => processNextCall()
      case Failure(_) => close()
    }
  }

  private var defaultBlockchainConfig: BlockchainConfigForEvm = _

  private def awaitHello(): Unit = {
    val helloMsg = messageHandler.awaitMessage[msg.Hello]
    //TODO: handle properly, read version from file
    require(helloMsg.version == "1.0")
    require(helloMsg.config.isEthereumConfig)
    defaultBlockchainConfig = constructBlockchainConfig(helloMsg.config.ethereumConfig.get)
  }

  def run(): Unit = {
    new Thread(() => {
      awaitHello()
      processNextCall()
    }).start()
  }

  def close(): Unit = {
    log.info("Connection closed")
    Try(in.cancel())
    Try(out.complete())
  }

  private def constructContextFromMsg(contextMsg: msg.CallContext): vm.ProgramContext[World, Storage] = {
    import ByteString.{empty => irrelevant} // used for irrelevant BlockHeader fields

    val blockHeader = BlockHeader(
      irrelevant,
      irrelevant,
      contextMsg.blockHeader.get.beneficiary,
      irrelevant,
      irrelevant,
      irrelevant,
      irrelevant,
      contextMsg.blockHeader.get.difficulty,
      contextMsg.blockHeader.get.number,
      contextMsg.blockHeader.get.gasLimit,
      0, // irrelevant
      contextMsg.blockHeader.get.unixTimestamp,
      irrelevant,
      irrelevant,
      irrelevant
    )

    val env = vm.ExecEnv(
      contextMsg.ownerAddr,
      contextMsg.callerAddr,
      contextMsg.originAddr,
      contextMsg.gasPrice,
      contextMsg.inputData,
      contextMsg.callValue,
      vm.Program(contextMsg.contractCode),
      blockHeader,
      contextMsg.callDepth
    )

    val blockchainConfig = contextMsg.config.ethereumConfig.map(constructBlockchainConfig).getOrElse(defaultBlockchainConfig)

    val vmConfig = vm.EvmConfig.forBlock(env.blockHeader.number, blockchainConfig)
    val world = World(blockchainConfig.accountStartNonce, vmConfig.noEmptyAccounts, messageHandler)

    vm.ProgramContext(env, contextMsg.ownerAddr, contextMsg.gasProvided, world, None, vmConfig)
  }

  private def buildResultMsg(result: ProgramResult[World, Storage]): msg.CallResult = {

    val logs = result.logs.map(l =>
      msg.LogEntry(address = l.loggerAddress, topics = l.logTopics.map(t => t: GByteString), data = l.data))

    msg.CallResult(
      returnData = result.returnData,
      gasRemaining = result.gasRemaining,
      gasRefund = result.gasRefund,
      error = result.error.isDefined,
      modifiedAccounts = buildModifiedAccountsMsg(result.world),
      deletedAccounts = result.addressesToDelete.toList.map(a => a: GByteString),
      touchedAccounts = result.world.touchedAccounts.toList.map(a => a: GByteString),
      logs = logs
    )
  }

  private def buildModifiedAccountsMsg(world: World): Seq[msg.ModifiedAccount] = {
    val modifiedAddresses = world.accounts.keySet ++ world.codeRepo.keySet ++ world.storages.keySet
    modifiedAddresses.toList.map { address =>
      val acc = world.getAccount(address)
      val storage = world.getStorage(address)
      val storageUpdates = storage.storage.map { case (key, value) => msg.StorageUpdate(key, value) }.toList

      msg.ModifiedAccount(
        address = address,
        nonce = acc.map(_.nonce: GByteString).getOrElse(GByteString.EMPTY),
        balance = acc.map(_.balance: GByteString).getOrElse(GByteString.EMPTY),
        storageUpdates = storageUpdates,
        code = world.getCode(address))
    }
  }

  private def constructBlockchainConfig(conf: msg.EthereumConfig): BlockchainConfigForEvm = {
    BlockchainConfigForEvm(
      frontierBlockNumber = conf.frontierBlockNumber,
      homesteadBlockNumber = conf.homesteadBlockNumber,
      eip106BlockNumber = conf.eip106BlockNumber,
      eip150BlockNumber = conf.eip150BlockNumber,
      eip155BlockNumber = conf.eip155BlockNumber,
      eip160BlockNumber = conf.eip160BlockNumber,
      eip161BlockNumber = conf.eip161BlockNumber,
      maxCodeSize = if (conf.maxCodeSize.isEmpty) None else Some(bigintFromGByteString(conf.maxCodeSize)),
      accountStartNonce = conf.accountStartNonce
    )
  }
}
