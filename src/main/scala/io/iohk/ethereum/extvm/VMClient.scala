package io.iohk.ethereum.extvm

import java.io.{InputStream, OutputStream}

import io.iohk.ethereum.vm.{Storage, WorldStateProxy, _}
import Implicits._
import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.utils.Logger

// scalastyle:off
class VMClient[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S], in: InputStream, out: OutputStream) extends Logger {

  private val world = context.world

  def run(): ProgramResult[W, S] = {
    val ctx = buildCallContextMsg(context)
    ctx.writeDelimitedTo(out)

    val callResult = messageLoop()
    constructResultFromMsg(world, callResult)
  }

  private def messageLoop(): msg.CallResult = {
    import msg.VMQuery.Query
    var result: msg.CallResult = null

    while(result eq null) {
      msg.VMQuery.parseDelimitedFrom(in).foreach { query =>
        query.query match {
          case Query.CallResult(res) =>
            log.debug("Client received msg: CallResult")
            result = res

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
            accountMsg.writeDelimitedTo(out)

          case Query.GetStorageData(msg.GetStorageData(address, offset)) =>
            log.debug("Client received msg: GetStorageData")
            val value = world.getStorage(address).load(offset)
            val storageDataMsg = msg.StorageData(data = value)
            storageDataMsg.writeDelimitedTo(out)

          case Query.GetCode(msg.GetCode(address)) =>
            log.debug("Client received msg: GetCode")
            val codeMsg = msg.Code(world.getCode(address))
            codeMsg.writeDelimitedTo(out)

          case Query.GetBlockhash(msg.GetBlockhash(offset)) =>
            log.debug("Client received msg: GetBlockhash")
            val blockhashMsg = world.getBlockHash(offset) match {
              case Some(value) => msg.Blockhash(hash = value)
              case None => msg.Blockhash()
            }
            blockhashMsg.writeDelimitedTo(out)

          case Query.Empty =>
            log.debug("Client received msg: Empty")
        }
      }
    }

    result
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
      receivingAddr = ctx.receivingAddr
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
