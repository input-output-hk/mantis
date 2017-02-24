package io.iohk.ethereum.vmrunner

import java.io.File

import akka.util.ByteString
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import io.iohk.ethereum.vm._
import io.iohk.ethereum.vmrunner.AST._
import io.iohk.ethereum.crypto.kec256
import org.spongycastle.util.encoders.Hex

import scala.io.Source


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
      val codeSize = State.world.getCode(xAcc.acc.codeHash).size
      val storageSize = State.world.getStorage(xAcc.acc.storageRoot).toMap.size

      s"""|  address:       ${xAcc.address}
          |  balance:       ${xAcc.acc.balance}
          |  code size:     $codeSize
          |  storage size:  $storageSize
       """.stripMargin
    }.map(Right(_)).getOrElse(Left(UnknownAccount(account)))

  def accountCode(account: String): Either[RunnerError, String] =
    State.world.getXAccount(account).map { xAccount =>
      val code = State.world.getCode(xAccount.acc.codeHash)
      printCode(code)
    }.map(Right(_)).getOrElse(Left(UnknownAccount(account)))

  def accountStorage(account: String): Either[RunnerError, String] =
    State.world.getXAccount(account).map { xAccount =>
      val storage = State.world.getStorage(xAccount.acc.storageRoot).toMap.toList.sortBy(_._1)
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
    args = call.args.map(s => DataWord(BigInt(s)).bytes)
    callData = args.foldLeft(sig)(_ ++ _)

    result = State.runTransaction(xAccount, callData, attr.gas, attr.value)
  } yield vmSummary(result)

  def createContract(name: String, attributes: Seq[Attr]): Either[RunnerError, String] = {
    val bin = new File(name + ".bin")
    val abi = new File(name + ".abi")

    if (!bin.exists())
      Left(ContractFileNotFound(name, "BIN"))
    else if (!abi.exists())
      Left(ContractFileNotFound(name, "ABI"))
    else {
      val codeHex = Source.fromFile(bin).mkString
      val abiJson = Source.fromFile(abi).mkString

      for {
        abis <- parseAbi(abiJson)
        attr <- getAttributes(attributes)
      } yield {
        val (addr, result) = State.createAccount(name, attr.value, attr.gas, loadCode(codeHex), abis.filter(_.`type` == "function"))
        val recap =
          if (result.error.isDefined) s"Failed to create contract $name"
          else s"Created contract: $name @ $addr"
        "  " + recap + "\n" + vmSummary(result)
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

  def parseAbi(input: String): Either[RunnerError, Seq[ABI]] = {
    implicit val config = Configuration.default.withDefaults
    decode[List[ABI]](input).left.map(e => JsonError(e.getMessage))
  }

  def loadCode(hexString: String): ByteString =
    ByteString(hexString.trim.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)

  def printCode(code: ByteString, i: Int = 0): String = {
    if (i >= code.size)
      ""
    else OpCode.byteToOpCode.get(code(i)) match {
      case Some(op: PushOp) =>
        val skip = op.code - PUSH1.code + 1
        val data = code.slice(i + 1, i + skip + 1).map(b => f"$b%02x").mkString(" ")
        s"  $op $data\n" + printCode(code, i + skip + 1)

      case Some(op) =>
        s"  $op\n" + printCode(code, i + 1)

      case None =>
        f"  0x${code(i)}%02x\n" + printCode(code, i + 1)
    }
  }

  def vmSummary(result: ProgramResult): String =
    s"""|  gas used:  ${result.gasUsed}
        |  error:     ${result.error.getOrElse("n/a")}
        |  return:    ${Hex.toHexString(result.returnData.toArray)}
     """.stripMargin

}
