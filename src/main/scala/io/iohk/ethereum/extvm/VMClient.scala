package io.iohk.ethereum.extvm

import io.iohk.ethereum.vm
import io.iohk.ethereum.vm.{WorldStateProxy, _}
import Implicits._
import akka.stream.scaladsl.{SinkQueueWithCancel, SourceQueueWithComplete}
import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}

import scala.annotation.tailrec
import scala.util.Try

class VMClient(
    in: SinkQueueWithCancel[ByteString],
    out: SourceQueueWithComplete[ByteString])
  extends Logger {

  private val messageHandler = new MessageHandler(in, out)

  def sendHello(version: String, blockchainConfig: BlockchainConfig): Unit = {
    val configMsg = msg.Hello.Config.EthereumConfig(buildEthereumConfigMsg(blockchainConfig))
    val helloMsg = msg.Hello(version, configMsg)
    messageHandler.sendMessage(helloMsg)
  }

  def run[W <: WorldStateProxy[W, S], S <: vm.Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
    val ctx = buildCallContextMsg(context)
    messageHandler.sendMessage(ctx)

    val callResult = messageLoop[W, S](context.world)
    constructResultFromMsg(context.world, callResult)
  }

  // scalastyle:off method.length
  @tailrec
  private def messageLoop[W <: WorldStateProxy[W, S], S <: vm.Storage[S]](world: W): msg.CallResult = {
    import msg.VMQuery.Query

    val nextMsg = messageHandler.awaitMessage[msg.VMQuery]

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
              codeEmpty = acc.codeHash == Account.EmptyCodeHash
            )

          case None =>
            msg.Account()
        }
        messageHandler.sendMessage(accountMsg)
        messageLoop[W, S](world)

      case Query.GetStorageData(msg.GetStorageData(address, offset)) =>
        log.debug("Client received msg: GetStorageData")
        val value = world.getStorage(address).load(offset)
        val storageDataMsg = msg.StorageData(data = value)
        messageHandler.sendMessage(storageDataMsg)
        messageLoop[W, S](world)

      case Query.GetCode(msg.GetCode(address)) =>
        log.debug("Client received msg: GetCode")
        val codeMsg = msg.Code(world.getCode(address))
        messageHandler.sendMessage(codeMsg)
        messageLoop[W, S](world)

      case Query.GetBlockhash(msg.GetBlockhash(offset)) =>
        log.debug("Client received msg: GetBlockhash")
        val blockhashMsg = world.getBlockHash(offset) match {
          case Some(value) => msg.Blockhash(hash = value)
          case None => msg.Blockhash()
        }
        messageHandler.sendMessage(blockhashMsg)
        messageLoop[W, S](world)

      case Query.Empty =>
        log.debug("Client received msg: Empty")
        messageLoop[W, S](world)
    }
  }

  private def constructResultFromMsg[W <: WorldStateProxy[W, S], S <: vm.Storage[S]](world: W, resultMsg: msg.CallResult): ProgramResult[W, S] = {
    val updatedWorld = applyAccountChanges[W, S](world, resultMsg)
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

  private def applyAccountChanges[W <: WorldStateProxy[W, S], S <: vm.Storage[S]](world: W, resultMsg: msg.CallResult): W = {
    val worldWithUpdatedAccounts = resultMsg.modifiedAccounts.foldLeft(world){ (w, change) =>
      val address: Address = change.address
      val initialStorage = w.getStorage(address)
      val updatedStorage = change.storageUpdates.foldLeft(initialStorage){ (s, update) =>
        s.store(update.offset, update.data)
      }

      val initialAccount = w.getAccount(address).getOrElse(w.getEmptyAccount)
      val updatedAccount = if (change.nonce.isEmpty) initialAccount else initialAccount.copy(nonce = change.nonce, balance = change.balance)

      val w1 = w.saveAccount(address, updatedAccount).saveStorage(address, updatedStorage)
      if (change.code.isEmpty) w1 else w1.saveCode(address, change.code)
    }

    worldWithUpdatedAccounts.touchAccounts(resultMsg.touchedAccounts.map(a => a: Address): _*)
  }

  private def buildCallContextMsg(ctx: ProgramContext[_, _]): msg.CallContext = {
    import msg.CallContext.Config
    val blockHeader = buildBlockHeaderMsg(ctx.env.blockHeader)
    val ethereumConfig = ctx.blockchainConfig.map(buildEthereumConfigMsg).map(Config.EthereumConfig)

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
      config = ethereumConfig.getOrElse(Config.Empty)
    )
  }

  private def buildEthereumConfigMsg(blockchainConfig: BlockchainConfig): msg.EthereumConfig =
    msg.EthereumConfig(
      frontierBlockNumber = blockchainConfig.frontierBlockNumber,
      homesteadBlockNumber = blockchainConfig.homesteadBlockNumber,
      eip106BlockNumber = blockchainConfig.eip106BlockNumber,
      eip150BlockNumber = blockchainConfig.eip150BlockNumber,
      eip155BlockNumber = blockchainConfig.eip155BlockNumber,
      eip160BlockNumber = blockchainConfig.eip160BlockNumber,
      eip161BlockNumber = blockchainConfig.eip161BlockNumber,
      maxCodeSize = blockchainConfig.maxCodeSize.map(bigintToGByteString).getOrElse(ByteString()),
      accountStartNonce = blockchainConfig.accountStartNonce
    )

  private def buildBlockHeaderMsg(header: BlockHeader): msg.BlockHeader =
    msg.BlockHeader(
      beneficiary = header.beneficiary,
      difficulty = header.difficulty,
      number = header.number,
      gasLimit = header.gasLimit,
      unixTimestamp = header.unixTimestamp
    )

  def close(): Unit = {
    Try(in.cancel())
    Try(out.complete())
  }

}
