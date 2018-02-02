package io.iohk.ethereum.extvm

import java.nio.ByteOrder

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Framing, Keep, Sink, SinkQueueWithCancel, Source, SourceQueueWithComplete, Tcp}
import akka.util.ByteString
import com.google.protobuf.{ByteString => GByteString}
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.domain.{Address, BlockHeader, UInt256}
import io.iohk.ethereum.extvm.Implicits._
import io.iohk.ethereum.utils._
import io.iohk.ethereum.vm
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

  private var defaultBlockchainConfig: msg.BlockchainConfig = _

  private def awaitDefaultBlockchainConfig(): Unit = {
    defaultBlockchainConfig = messageHandler.awaitMessage[msg.BlockchainConfig]
  }

  def run(): Unit = {
    new Thread(() => {
      awaitDefaultBlockchainConfig()
      processNextCall()
    }).start()
  }

  def close(): Unit = {
    log.info("Connection closed")
    Try(in.cancel())
    Try(out.complete())
  }

  private def constructContextFromMsg(contextMsg: msg.CallContext): vm.ProgramContext[World, Storage] = {
    val blockHeader = BlockHeader(
      contextMsg.blockHeader.get.parentHash,
      contextMsg.blockHeader.get.ommersHash,
      contextMsg.blockHeader.get.beneficiary,
      contextMsg.blockHeader.get.stateRoot,
      contextMsg.blockHeader.get.transactionsRoot,
      contextMsg.blockHeader.get.receiptsRoot,
      contextMsg.blockHeader.get.logsBloom,
      contextMsg.blockHeader.get.difficulty,
      contextMsg.blockHeader.get.number,
      contextMsg.blockHeader.get.gasLimit,
      contextMsg.blockHeader.get.gasUsed,
      contextMsg.blockHeader.get.unixTimestamp,
      contextMsg.blockHeader.get.extraData,
      contextMsg.blockHeader.get.mixHash,
      contextMsg.blockHeader.get.nonce
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

    val blockchainConfig = constructBlockchainConfig(contextMsg.blockchainConfig.getOrElse(defaultBlockchainConfig))

    val vmConfig = vm.EvmConfig.forBlock(env.blockHeader.number, blockchainConfig)
    val world = World(blockchainConfig.accountStartNonce, vmConfig.noEmptyAccounts, messageHandler)

    vm.ProgramContext(env, contextMsg.receivingAddr, contextMsg.gasProvided, world, None, vmConfig)
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

  private def constructBlockchainConfig(conf: msg.BlockchainConfig): BlockchainConfig = {
    new BlockchainConfig {
      override val frontierBlockNumber: BigInt = conf.frontierBlockNumber
      override val homesteadBlockNumber: BigInt = conf.homesteadBlockNumber
      override val eip106BlockNumber: BigInt = conf.eip106BlockNumber
      override val eip150BlockNumber: BigInt = conf.eip150BlockNumber
      override val eip155BlockNumber: BigInt = conf.eip155BlockNumber
      override val eip160BlockNumber: BigInt = conf.eip160BlockNumber
      override val eip161BlockNumber: BigInt = conf.eip161BlockNumber
      override val maxCodeSize: Option[BigInt] = if (conf.maxCodeSize.isEmpty) None else Some(bigintFromGByteString(conf.maxCodeSize))
      override val difficultyBombPauseBlockNumber: BigInt = conf.difficultyBombPauseBlockNumber
      override val difficultyBombContinueBlockNumber: BigInt = conf.difficultyBombContinueBlockNumber

      override val customGenesisFileOpt: Option[String] = None

      override val daoForkConfig = if (conf.forkBlockHash.isEmpty) None else Some(new DaoForkConfig {
        override val blockExtraData: Option[ByteString] = if (conf.forkBlockExtraData.isEmpty) None else Some(conf.forkBlockExtraData)
        override val range: Int = conf.forkRange
        override val drainList: Seq[Address] = conf.forkDrainList.map(addressFromGByteString)
        override val forkBlockHash: ByteString = conf.forkBlockHash
        override val forkBlockNumber: BigInt = conf.forkBlockNumber
        override val refundContract: Option[Address] = if (conf.forkRefundContract.isEmpty) None else Some(conf.forkRefundContract)
      })

      override val accountStartNonce: UInt256 = conf.accountStartNonce

      override val chainId: Byte = conf.chainId.head

      override val monetaryPolicyConfig = MonetaryPolicyConfig(
        eraDuration = conf.eraDuration,
        rewardReductionRate = conf.rewardReductionRate,
        firstEraBlockReward = conf.firstEraBlockReward)

      val gasTieBreaker: Boolean = conf.gasTieBreaker
    }
  }
}
