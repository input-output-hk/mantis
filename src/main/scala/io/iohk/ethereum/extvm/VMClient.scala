package io.iohk.ethereum.extvm

import java.math.BigInteger

import io.iohk.ethereum.vm.{Storage, WorldStateProxy, _}
import Implicits._
import akka.stream.scaladsl.{SinkQueueWithCancel, SourceQueueWithComplete}
import akka.util.ByteString
import com.google.protobuf.CodedInputStream
import com.trueaccord.scalapb.LiteParser
import io.iohk.ethereum.domain._
import io.iohk.ethereum.utils.{BlockchainConfig, ByteUtils, Logger}
import org.spongycastle.util.BigIntegers

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// scalastyle:off
class VMClient[W <: WorldStateProxy[W, S], S <: Storage[S]](blockchainConfig: BlockchainConfig, context: ProgramContext[W, S], in: SinkQueueWithCancel[ByteString], out: SourceQueueWithComplete[ByteString]) extends Logger {

  private val world = context.world

  def run(): ProgramResult[W, S] = {
    val ctx = buildCallContextMsg(context)
    sendMessage(ctx)

    val callResult = messageLoop()
    constructResultFromMsg(world, callResult)
  }

  def sendMessage[M <: com.trueaccord.scalapb.GeneratedMessage](msg: M): Unit = {
    val bytes = msg.toByteArray
    val lengthBytes = ByteString(BigIntegers.asUnsignedByteArray(4, BigInteger.valueOf(bytes.length)))

    out offer (lengthBytes ++ ByteString(bytes))
  }

  def awaitMessage[M <: com.trueaccord.scalapb.GeneratedMessage with com.trueaccord.scalapb.Message[M]](implicit companion: com.trueaccord.scalapb.GeneratedMessageCompanion[M]): M = {
    val resF = in.pull() map {
      case Some(bytes) => LiteParser.parseFrom(companion, CodedInputStream.newInstance(bytes.toArray[Byte]))
      case None => throw new RuntimeException("Stream completed")
    }

    Await.result(resF, 1.minute)
  }

  @tailrec
  private def messageLoop(): msg.CallResult = {
    import msg.VMQuery.Query

    val nextMsg = awaitMessage[msg.VMQuery]

    nextMsg.query match {
      case Query.CallResult(res) =>
        log.debug("Client received msg: CallResult")
        res

      case Query.GetAccount(msg.GetAccount(address)) =>
        log.debug("Client received msg: GetAccount")
        val accountMsg = world.getAccount(address) match {
          case Some(acc) =>
            msg.Account(
              nonce = acc.nonce,
              balance = acc.balance,
              storage = acc.storageRoot
            )

          case None =>
            msg.Account()
        }
        sendMessage(accountMsg)
        messageLoop()

      case Query.GetStorageData(msg.GetStorageData(address, offset)) =>
        log.debug("Client received msg: GetStorageData")
        val value = world.getStorage(address).load(offset)
        val storageDataMsg = msg.StorageData(data = value)
        sendMessage(storageDataMsg)
        messageLoop()

      case Query.GetCode(msg.GetCode(address)) =>
        log.debug("Client received msg: GetCode")
        val codeMsg = msg.Code(world.getCode(address))
        sendMessage(codeMsg)
        messageLoop()

      case Query.GetBlockhash(msg.GetBlockhash(offset)) =>
        log.debug("Client received msg: GetBlockhash")
        val blockhashMsg = world.getBlockHash(offset) match {
          case Some(value) => msg.Blockhash(hash = value)
          case None => msg.Blockhash()
        }
        sendMessage(blockhashMsg)
        messageLoop()

      case Query.Empty =>
        log.debug("Client received msg: Empty")
        messageLoop()
    }
  }

  private def constructResultFromMsg(world: W, resultMsg: msg.CallResult): ProgramResult[W, S] = {
    val updatedWorld = applyAccountChanges(world, resultMsg)
    ProgramResult(
      resultMsg.returnData,
      resultMsg.gasRemaining,
      updatedWorld,
      resultMsg.deletedAccounts.map(a => a: Address).toSet,
      resultMsg.logs.map(l => TxLogEntry(l.address, l.topics.map(t => t: ByteString), l.data)),
      Nil,
      resultMsg.gasRefund,
      if (resultMsg.error) Some(OutOfGas) else None
    )
  }

  private def applyAccountChanges(world: W, resultMsg: msg.CallResult): W = {
    val worldWithUpdatedAccounts = resultMsg.modifiedAccounts.foldLeft(world){ (w, change) =>
      val address: Address = change.address
      val intialStorage = w.getStorage(address)
      val updatedStorage = change.storageUpdates.foldLeft(intialStorage){ (s, update) =>
        s.store(update.offset, update.data)
      }

      val account = Account(change.nonce, change.balance)
      val w1 = w.saveAccount(address, account).saveStorage(address, updatedStorage)
      if (change.code.isEmpty) w1 else w1.saveCode(address, change.code)
    }

    worldWithUpdatedAccounts.touchAccounts(resultMsg.touchedAccounts.map(a => a: Address): _*)
  }

  private def buildCallContextMsg(ctx: ProgramContext[_, _]): msg.CallContext = {
    val blockHeader = buildBlockHeaderMsg(ctx.env.blockHeader)
    msg.CallContext(
      ownerAddr = ctx.env.ownerAddr,
      callerAddr = ctx.env.callerAddr,
      originAddr = ctx.env.originAddr,
      contractCode = ctx.env.program.code,
      inputData = ctx.env.inputData,
      callValue = ctx.env.value,
      gasPrice = ctx.env.gasPrice,
      gasProvided = ctx.startGas,
      callDepth = ctx.env.callDepth,
      blockHeader = Some(blockHeader),
      receivingAddr = ctx.receivingAddr,

      frontierBlockNumber = blockchainConfig.frontierBlockNumber,
      homesteadBlockNumber = blockchainConfig.homesteadBlockNumber,
      eip106BlockNumber = blockchainConfig.eip106BlockNumber,
      eip150BlockNumber = blockchainConfig.eip150BlockNumber,
      eip155BlockNumber = blockchainConfig.eip155BlockNumber,
      eip160BlockNumber = blockchainConfig.eip160BlockNumber,
      eip161BlockNumber = blockchainConfig.eip161BlockNumber,
      maxCodeSize = blockchainConfig.maxCodeSize.map(bigintToGByteString).getOrElse(ByteString()),
      difficultyBombPauseBlockNumber = blockchainConfig.difficultyBombPauseBlockNumber,
      difficultyBombContinueBlockNumber = blockchainConfig.difficultyBombContinueBlockNumber,

      forkBlockNumber = blockchainConfig.daoForkConfig.map(f => bigintToGByteString(f.forkBlockNumber)).getOrElse(ByteString()),
      forkBlockHash = byteString2GByteString(blockchainConfig.daoForkConfig.map(_.forkBlockHash).getOrElse(ByteString())),
      forkBlockExtraData = byteString2GByteString(blockchainConfig.daoForkConfig.flatMap(_.blockExtraData).getOrElse(ByteString())),

      forkRefundContract = address2GByteString(blockchainConfig.daoForkConfig.flatMap(_.refundContract).getOrElse(Address(0x0))),
      forkDrainList = blockchainConfig.daoForkConfig.map(_.drainList.map(address2GByteString)).getOrElse(Seq.empty),

      accountStartNonce = blockchainConfig.accountStartNonce,
      chainId = ByteString(blockchainConfig.chainId),

      eraDuration = blockchainConfig.monetaryPolicyConfig.eraDuration,
      rewardReductionRate = blockchainConfig.monetaryPolicyConfig.rewardReductionRate,
      firstEraBlockReward = blockchainConfig.monetaryPolicyConfig.firstEraBlockReward,

      gasTieBreaker = blockchainConfig.gasTieBreaker
    )
  }

  private def buildBlockHeaderMsg(header: BlockHeader): msg.BlockHeader =
    msg.BlockHeader(
      parentHash = header.parentHash,
      ommersHash = header.ommersHash,
      beneficiary = header.beneficiary,
      stateRoot = header.stateRoot,
      transactionsRoot = header.transactionsRoot,
      receiptsRoot = header.receiptsRoot,
      logsBloom = header.logsBloom,
      difficulty = header.difficulty,
      number = header.number,
      gasLimit = header.gasLimit,
      gasUsed = header.gasUsed,
      unixTimestamp = header.unixTimestamp,
      extraData = header.extraData,
      mixHash = header.mixHash,
      nonce = header.nonce
    )
}
