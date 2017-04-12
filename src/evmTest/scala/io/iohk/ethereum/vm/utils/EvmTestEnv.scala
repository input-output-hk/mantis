package io.iohk.ethereum.vm.utils

import java.io.File

import scala.language.dynamics
import scala.util.Random

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.vmrunner.{ABI, MockVmInput}
import io.iohk.ethereum.vmrunner.Utils
import io.iohk.ethereum.{rlp, crypto}
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.vm._

object EvmTestEnv {
  val ContractsDir = new File("target/contracts")
}

// scalastyle:off magic.number
trait EvmTestEnv {

  import EvmTestEnv._

  private var contractsAddresses: Map[String, Address] = Map.empty
  private var contractsAbis: Map[String, Seq[ABI]] = Map.empty

  private var internalWorld = MockWorldState()

  val defaultContractCreator = createAccount(balance = 1000)
  val defaultSender = createAccount(balance = 1000)

  def world: MockWorldState = internalWorld

  def contract(name: String): Contract = {
    new Contract(name, contractsAddresses(name))
  }

  def createAccount(balance: BigInt = 0): Address = {
    val newAddress = Address(Random.nextLong())
    internalWorld = world.saveAccount(newAddress, Account.Empty.copy(balance = UInt256(balance)))
    newAddress
  }

  def deployContract(name: String,
                     creatorAddress: Address = defaultContractCreator,
                     value: BigInt = 0,
                     gasLimit: BigInt = BigInt(2).pow(256) - 1,
                     gasPrice: BigInt = 1,
                     constructorArgs: Seq[Any] = Nil): (ProgramResult[MockWorldState, MockStorage], Contract) = {
    val creator = world.getAccount(creatorAddress).get

    val contractAddress = {
      val hash = crypto.kec256(rlp.encode(RLPList(creatorAddress.bytes, creator.nonce)))
      Address(hash.takeRight(Address.Length))
    }

    val contractInitCode = Utils.loadContractCodeFromFile(new File(s"$ContractsDir/$name.bin"))
    val contractAbi = Utils.loadContractAbiFromFile(new File(s"$ContractsDir/$name.abi"))

    val payload = constructorArgs.map(parseArg).foldLeft(contractInitCode)(_ ++ _)

    val tx = MockVmInput.transaction(creatorAddress, payload, value, gasLimit, gasPrice)
    val bh = MockVmInput.blockHeader

    val context = ProgramContext[MockWorldState, MockStorage](tx, bh, world)
    val result = VM.run(context)

    contractsAbis += (name -> contractAbi.right.get)
    contractsAddresses += (name -> contractAddress)

    internalWorld = result.world
      .saveAccount(creatorAddress, creator.increaseNonce)
      .saveAccount(contractAddress, Account(UInt256(0), UInt256(value), Account.EmptyStorageRootHash, kec256(result.returnData)))
      .saveCode(contractAddress, result.returnData)

    (result, new Contract(name, contractAddress))
  }

  private def parseArg(arg: Any): ByteString = arg match {
    case b: ByteString => UInt256(b).bytes
    case b: BigInt => UInt256(b).bytes
    case a: Array[Byte] => UInt256(a).bytes
    case i: Int => UInt256(i).bytes
    case b: Byte => UInt256(b).bytes
    case a: Address => UInt256(a.bytes).bytes
    case other => throw new RuntimeException("Invalid call argument")
  }

  class Contract(val name: String, val address: Address) extends Dynamic {

    def applyDynamic(methodName: String)(args: Any*): ContractMethodCall = {
      callMethod(methodName)(args: _*)
    }

    def callMethod(methodName: String)(args: Any*): ContractMethodCall = {
      if (methodName.contains("("))
        callWithSignature(methodName)(args: _*)
      else
        callWithMethodName(methodName)(args: _*)
    }

    private def callWithMethodName(methodName: String)(args: Any*): ContractMethodCall = {
      val matchedAbis = contractsAbis(name)
        .filter { a => a.inputs.size == args.size && a.name == methodName && a.`type` == "function" }

      if (matchedAbis.isEmpty)
        throw new RuntimeException("No matching ABI found. Please specify the signature")
      else if (matchedAbis.size > 1)
        throw new RuntimeException("More than one matching ABI found. Please specify the signature")
      else {
        val abi = matchedAbis.head
        callWithSignature(s"$methodName(${abi.inputs.map(_.`type`).mkString(",")})")(args: _*)
      }
    }

    private def callWithSignature(signature: String)(args: Any*): ContractMethodCall = {
      val signatureHash = ByteString(crypto.kec256(signature.getBytes)).take(4)
      val callData = args.map(parseArg).foldLeft(signatureHash)(_ ++ _)
      new ContractMethodCall(this, callData)
    }

    def storage: MockStorage = world.storages(address)
  }

  class ContractMethodCall(contract: Contract, callData: ByteString) {
    def call(value: BigInt = 0,
             gasLimit: BigInt = BigInt(2).pow(256) - 1,
             gasPrice: BigInt = 1,
             sender: Address = defaultSender): ProgramResult[MockWorldState, MockStorage] = {
      val transaction = MockVmInput.transaction(sender, callData, value, gasLimit, gasPrice, Some(contract.address))
      val blockHeader = MockVmInput.blockHeader
      val pc = ProgramContext[MockWorldState, MockStorage](transaction, blockHeader, world)

      val res = VM.run(pc)
      internalWorld = res.world
      res
    }
  }

}
