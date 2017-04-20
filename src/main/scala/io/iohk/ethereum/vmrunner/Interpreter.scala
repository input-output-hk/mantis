package io.iohk.ethereum.vmrunner

import java.io.File

import akka.util.ByteString
import io.iohk.ethereum.vm._
import io.iohk.ethereum.vmrunner.AST._
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.vmrunner.WorldState.PR
import org.spongycastle.util.encoders.Hex

object Interpreter {

  case class CallAttributes(gas: BigInt, value: BigInt)

  def apply(cmd: Cmd): Either[RunnerError, String] = cmd match {
    case DoNothing =>
      Right("")

    case Create(name, attrs) =>
      createContract(name, attrs)

    case Accounts =>
      Right(accountsInfo())

    case Object(target) =>
      accountInfo(target)

    case Property(target, "code") =>
      accountCode(target)

    case Property(target, "storage") =>
      accountStorage(target)

    case Property(target, "functions") =>
      listFunctions(target)

    case Property(target, "balance") =>
      getBalance(target)

    case Property(_, invalid) =>
      Left(InvalidProperty(invalid))

    case call: FunCall =>
      performFunCall(call)
  }


  def accountsInfo(): String =
    State.world.listAccounts.map { xAcc =>
      f"  ${xAcc.name}%-16s @ ${xAcc.address}"
    }.mkString("\n")

  def accountInfo(account: String): Either[RunnerError, String] =
    State.world.getXAccount(account).map { xAcc =>
      val codeSize = State.world.getCode(xAcc.address).size
      val storageSize = State.world.getStorage(xAcc.address).data.size

      s"""|  address:       ${xAcc.address}
          |  balance:       ${xAcc.acc.balance}
          |  code size:     $codeSize
          |  storage size:  $storageSize
       """.stripMargin
    }.map(Right(_)).getOrElse(Left(UnknownAccount(account)))

  def accountCode(account: String): Either[RunnerError, String] =
    State.world.getXAccount(account).map { xAccount =>
      val code = State.world.getCode(xAccount.address)
      printCode(code)
    }.map(Right(_)).getOrElse(Left(UnknownAccount(account)))

  def accountStorage(account: String): Either[RunnerError, String] =
    State.world.getXAccount(account).map { xAccount =>
      val storage = State.world.getStorage(xAccount.address).data.toList.sortBy(_._1)
      if (storage.isEmpty)
        "  empty"
      else
        storage.map { case (key, value) =>
          f"0x${key.toBigInt}%02x: 0x${value.toBigInt}%02x"
        }.mkString("  ", "\n  ", "")
    }.map(Right(_)).getOrElse(Left(UnknownAccount(account)))

  def listFunctions(account: String): Either[RunnerError, String] =
    State.world.getXAccount(account).map { xAccount =>
      xAccount.abis.map(_.fullSignature).mkString("  ", "\n  ", "")
    }.map(Right(_)).getOrElse(Left(UnknownAccount(account)))

  def getBalance(account: String): Either[RunnerError, String] =
    State.world.getXAccount(account).map { xAccount =>
      "  " + xAccount.acc.balance
    }.map(Right(_)).getOrElse(Left(UnknownAccount(account)))

  def performFunCall(call: FunCall): Either[RunnerError, String] = for {
    xAccount <- State.world.getXAccount(call.target).map(Right(_)).getOrElse(Left(UnknownAccount(call.target)))
    abi <- xAccount.abis.find(a => a.name == call.name && a.inputs.size == call.args.size).map(Right(_)).getOrElse(Left(UnknownFunction(call)))
    attr <- getAttributes(call.attrs)

    sig = ByteString(kec256(abi.shortSignature.getBytes)).take(4)
    args = call.args.map(s => UInt256(BigInt(s)).bytes)
    callData = args.foldLeft(sig)(_ ++ _)

    result = State.runTransaction(xAccount, callData, attr.gas, attr.value)
  } yield vmSummary(attr.gas, result)

  def createContract(name: String, attributes: Seq[Attr]): Either[RunnerError, String] = {
    val binFile = new File(name + ".bin")
    val abiFile = new File(name + ".abi")

    if (!binFile.exists())
      Left(ContractFileNotFound(name, "BIN"))
    else if (!abiFile.exists())
      Left(ContractFileNotFound(name, "ABI"))
    else {
      val code = Utils.loadContractCodeFromFile(binFile)

      for {
        abis <- Utils.loadContractAbiFromFile(abiFile).left.map(e => JsonError(e.getMessage))
        attr <- getAttributes(attributes)
      } yield {
        val (addr, result) = State.createAccount(name, attr.value, attr.gas, code, abis.filter(_.`type` == "function"))
        val recap =
          if (result.error.isDefined) s"Failed to create contract $name"
          else s"Created contract: $name @ $addr"
        "  " + recap + "\n" + vmSummary(attr.gas, result)
      }
    }
  }

  def getAttributes(attrs: Seq[Attr]): Either[RunnerError, CallAttributes] = {
    val validAttributes = List("gas", "value")
    val invalidAttributes = attrs.map(_.name).diff(validAttributes)
    if (invalidAttributes.nonEmpty)
      Left(InvalidAttribute(invalidAttributes.head))
    else {
      val attrMap = attrs.map(a => a.name -> a.value).toMap
      val attributes = CallAttributes(
        gas = attrMap.getOrElse("gas", BigInt(2).pow(256) - 1),
        value = attrMap.getOrElse("value", 0)
      )
      Right(attributes)
    }
  }

  def printCode(code: ByteString, i: Int = 0): String = {
    if (i >= code.size)
      ""
    else {
      val opcode = EvmConfig.PostEIP160Config.byteToOpCode.get(code(i)) match {
        case Some(op: PushOp) =>
          val skip = op.code - PUSH1.code + 1
          val data = code.slice(i + 1, i + skip + 1).map(b => f"$b%02x").mkString(" ")
          s"$op $data\n" + printCode(code, i + skip + 1)

        case Some(op) =>
          s"$op\n" + printCode(code, i + 1)

        case None =>
          f"0x${code(i)}%02x\n" + printCode(code, i + 1)
      }

      f"  $i%04x:    " + opcode
    }
  }

  def vmSummary(initialGas: BigInt, result: PR): String =
    s"""|  gas used:  ${initialGas - result.gasRemaining}
        |  error:     ${result.error.getOrElse("n/a")}
        |  return:    ${Hex.toHexString(result.returnData.toArray)}
     """.stripMargin

}
