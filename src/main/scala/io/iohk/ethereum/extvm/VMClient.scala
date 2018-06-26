package io.iohk.ethereum.extvm

import io.iohk.ethereum.vm
import io.iohk.ethereum.vm.{WorldStateProxy, _}
import Implicits._
import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.utils.{BlockchainConfig, Logger, VmConfig}

import scala.annotation.tailrec

/**
  * @param testMode - if enabled the client will send blockchain configuration with each configuration.
  *                 This is useful to override configuration for each test, rather than to recreate the VM.
  */
class VMClient(
    externalVmConfig: VmConfig.ExternalConfig,
    messageHandler: MessageHandler,
    testMode: Boolean)
  extends Logger {

  def sendHello(version: String, blockchainConfig: BlockchainConfig): Unit = {
    val config = BlockchainConfigForEvm(blockchainConfig)
    val configMsg = externalVmConfig.vmType match {
      case VmConfig.ExternalConfig.VmTypeIele => msg.Hello.Config.IeleConfig(buildIeleConfigMsg())
      case _ => msg.Hello.Config.EthereumConfig(buildEthereumConfigMsg(config))
    }
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
            msg.Account(codeEmpty = true)
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
      if (resultMsg.error) Some(WithReturnCode(resultMsg.returnCode)) else None
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
    val blockHeader = buildBlockHeaderMsg(ctx.blockHeader)

    val config = externalVmConfig.vmType match {
      case VmConfig.ExternalConfig.VmTypeIele => Config.IeleConfig(buildIeleConfigMsg()) // always pass config for IELE
      case VmConfig.ExternalConfig.VmTypeKevm => Config.EthereumConfig(buildEthereumConfigMsg(ctx.evmConfig.blockchainConfig))  // always pass config for KEVM
      case _ if testMode => Config.EthereumConfig(buildEthereumConfigMsg(ctx.evmConfig.blockchainConfig))
      case _ => Config.Empty
    }

    msg.CallContext(
      callerAddr = ctx.callerAddr,
      recipientAddr = ctx.recipientAddr.map(_.bytes).getOrElse(ByteString.empty): ByteString,
      inputData = ctx.inputData,
      callValue = ctx.value,
      gasPrice = ctx.gasPrice,
      gasProvided = ctx.startGas,
      blockHeader = Some(blockHeader),
      config = config
    )
  }

  private def buildEthereumConfigMsg(blockchainConfig: BlockchainConfigForEvm): msg.EthereumConfig =
    msg.EthereumConfig(
      frontierBlockNumber = blockchainConfig.frontierBlockNumber,
      homesteadBlockNumber = blockchainConfig.homesteadBlockNumber,
      eip150BlockNumber = blockchainConfig.eip150BlockNumber,
      eip160BlockNumber = blockchainConfig.eip160BlockNumber,
      eip161BlockNumber = blockchainConfig.eip161BlockNumber,
      maxCodeSize = blockchainConfig.maxCodeSize.map(bigintToGByteString).getOrElse(ByteString()),
      accountStartNonce = blockchainConfig.accountStartNonce
    )

  private def buildIeleConfigMsg(): msg.IeleConfig =
    msg.IeleConfig()

  private def buildBlockHeaderMsg(header: BlockHeader): msg.BlockHeader =
    msg.BlockHeader(
      beneficiary = header.beneficiary,
      difficulty = header.difficulty,
      number = header.number,
      gasLimit = header.gasLimit,
      unixTimestamp = header.unixTimestamp
    )

  def close(): Unit = {
    messageHandler.close()
  }

}
