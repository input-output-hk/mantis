package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.{Message, MessageSerializableImplicit}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.vm.{ExecEnv, Program}
import org.spongycastle.util.encoders.Hex


// scalastyle:off
object VmMessages {

  import CommonMessages._
  import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._
  import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
  import io.iohk.ethereum.network.p2p.messages.PV62._
  import BlockHeaderImplicits._
  import SignedTransactions._
  import UInt256RLPImplicits._
  import io.iohk.ethereum.network.p2p.messages.PV63.AccountImplicits._


  object StorageData {
    val code: Int = Versions.SubProtocolOffset + 0x21
  }

  implicit class StorageDataEnc(val underlyingMsg: StorageData) extends MessageSerializableImplicit[StorageData](underlyingMsg) with RLPSerializable {
    override def code: Int = Execute.code

    override def toRLPEncodable: RLPEncodeable = {
      import msg._
      RLPList(executionId, data.toRLPEncodable)
    }
  }

  implicit class StorageDataDec(val bytes: Array[Byte]) extends AnyVal {
    def toStorageData: StorageData = rawDecode(bytes) match {
      case RLPList(executionId, data) =>
        StorageData(executionId, data.toUInt256)
      case _ => throw new RuntimeException("Cannot decode Status")
    }
  }

  case class StorageData(executionId: Long, data: UInt256) extends Message {
    override def code: Int = StorageData.code
  }




  object GetStorageData {
    val code: Int = Versions.SubProtocolOffset + 0x20
  }

  implicit class GetStorageDataEnc(val underlyingMsg: GetStorageData) extends MessageSerializableImplicit[GetStorageData](underlyingMsg) with RLPSerializable {
    override def code: Int = GetStorageData.code

    override def toRLPEncodable: RLPEncodeable = {
      import msg._
      RLPList(executionId, address.toRLPEncodable, offset.toRLPEncodable)
    }
  }

  implicit class GetStorageDataDec(val bytes: Array[Byte]) extends AnyVal {
    def toGetStorageData: GetStorageData = rawDecode(bytes) match {
      case RLPList(executionId, address, offset) =>
        GetStorageData(executionId, address.toAddress, offset.toUInt256)
      case _ => throw new RuntimeException("Cannot decode Status")
    }
  }

  case class GetStorageData(executionId: Long, address: Address, offset: UInt256) extends Message {
    override def code: Int = GetStorageData.code
  }



  object GetAccount {
    val code: Int = Versions.SubProtocolOffset + 0x22
  }

  implicit class GetAccountEnc(val underlyingMsg: GetAccount) extends MessageSerializableImplicit[GetAccount](underlyingMsg) with RLPSerializable {
    override def code: Int = GetAccount.code

    override def toRLPEncodable: RLPEncodeable = {
      import msg._
      RLPList(executionId, address.toRLPEncodable)
    }
  }

  implicit class GetAccountDec(val bytes: Array[Byte]) extends AnyVal {
    def toGetAccount: GetAccount = rawDecode(bytes) match {
      case RLPList(executionId, address) =>
        GetAccount(executionId, address.toAddress)
      case _ => throw new RuntimeException("Cannot decode Status")
    }
  }

  case class GetAccount(executionId: Long, address: Address) extends Message {
    override def code: Int = GetAccount.code
  }



  object AccountResponse {
    val code: Int = Versions.SubProtocolOffset + 0x23
  }

  implicit class AccountResponseEnc(val underlyingMsg: AccountResponse) extends MessageSerializableImplicit[AccountResponse](underlyingMsg) with RLPSerializable {
    override def code: Int = AccountResponse.code

    override def toRLPEncodable: RLPEncodeable = {
      import msg._
      RLPList(executionId, account.toRLPEncodable)
    }
  }

  implicit class AccountResponseDec(val bytes: Array[Byte]) extends AnyVal {
    def toAccountResponse: AccountResponse = rawDecode(bytes) match {
      case RLPList(executionId, account) =>
        AccountResponse(executionId, AccountDec(account).toAccount)
      case _ => throw new RuntimeException("Cannot decode Status")
    }
  }

  case class AccountResponse(executionId: Long, account: Account) extends Message {
    override def code: Int = AccountResponse.code
  }





  object GetCode {
    val code: Int = Versions.SubProtocolOffset + 0x24
  }

  implicit class GetCodeEnc(val underlyingMsg: GetCode) extends MessageSerializableImplicit[GetCode](underlyingMsg) with RLPSerializable {
    override def code: Int = GetCode.code

    override def toRLPEncodable: RLPEncodeable = {
      import msg._
      RLPList(executionId, address.toRLPEncodable)
    }
  }

  implicit class GetCodeDec(val bytes: Array[Byte]) extends AnyVal {
    def toGetCode: GetCode = rawDecode(bytes) match {
      case RLPList(executionId, address) =>
        GetCode(executionId, address.toAddress)
      case _ => throw new RuntimeException("Cannot decode Status")
    }
  }

  case class GetCode(executionId: Long, address: Address) extends Message {
    override def code: Int = GetCode.code
  }


  object CodeResponse {
    val code: Int = Versions.SubProtocolOffset + 0x25
  }

  implicit class CodeResponseEnc(val underlyingMsg: CodeResponse) extends MessageSerializableImplicit[CodeResponse](underlyingMsg) with RLPSerializable {
    override def code: Int = CodeResponse.code

    override def toRLPEncodable: RLPEncodeable = {
      import msg._
      RLPList(executionId, data)
    }
  }

  implicit class CodeResponseDec(val bytes: Array[Byte]) extends AnyVal {
    def toCodeResponse: CodeResponse = rawDecode(bytes) match {
      case RLPList(executionId, data) =>
        CodeResponse(executionId, data)
      case _ => throw new RuntimeException("Cannot decode Status")
    }
  }

  case class CodeResponse(executionId: Long, data: ByteString) extends Message {
    override def code: Int = CodeResponse.code
  }



  object GetBlockHash {
    val code: Int = Versions.SubProtocolOffset + 0x26
  }

  implicit class GetBlockHashEnc(val underlyingMsg: GetBlockHash) extends MessageSerializableImplicit[GetBlockHash](underlyingMsg) with RLPSerializable {
    override def code: Int = GetBlockHash.code

    override def toRLPEncodable: RLPEncodeable = {
      import msg._
      RLPList(executionId, number)
    }
  }

  implicit class GetBlockHashDec(val bytes: Array[Byte]) extends AnyVal {
    def toGetBlockHash: GetBlockHash = rawDecode(bytes) match {
      case RLPList(executionId, number) =>
        GetBlockHash(executionId, number)
      case _ => throw new RuntimeException("Cannot decode Status")
    }
  }

  case class GetBlockHash(executionId: Long, number: BigInt) extends Message {
    override def code: Int = GetBlockHash.code
  }



  object BlockHashResponse {
    val code: Int = Versions.SubProtocolOffset + 0x27
  }

  implicit class BlockHashResponseEnc(val underlyingMsg: BlockHashResponse) extends MessageSerializableImplicit[BlockHashResponse](underlyingMsg) with RLPSerializable {
    override def code: Int = BlockHashResponse.code

    override def toRLPEncodable: RLPEncodeable = {
      import msg._
      RLPList(executionId, hash)
    }
  }

  implicit class BlockHashResponseDec(val bytes: Array[Byte]) extends AnyVal {
    def toBlockHashResponse: BlockHashResponse = rawDecode(bytes) match {
      case RLPList(executionId, hash) =>
        BlockHashResponse(executionId, hash)
      case _ => throw new RuntimeException("Cannot decode Status")
    }
  }

  case class BlockHashResponse(executionId: Long, hash: ByteString) extends Message {
    override def code: Int = BlockHashResponse.code
  }





  object Execute {
    val code: Int = Versions.SubProtocolOffset + 0x19
  }

  implicit class ExecuteEnc(val underlyingMsg: Execute) extends MessageSerializableImplicit[Execute](underlyingMsg) with RLPSerializable {
    override def code: Int = Execute.code

    override def toRLPEncodable: RLPEncodeable = {
      import msg._
      RLPList(executionId, context.toRLPEncodable)
    }
  }

  implicit class ExecuteDec(val bytes: Array[Byte]) extends AnyVal {
    def toExecute: Execute = rawDecode(bytes) match {
      case RLPList(executionId, context) =>
        Execute(executionId, context.toContext)
      case _ => throw new RuntimeException("Cannot decode Status")
    }
  }

  case class Execute(executionId: Long, context: Context) extends Message {
    override def code: Int = Execute.code
  }

    implicit class ContextEnc(val underlyingMsg: Context) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import underlyingMsg._
        RLPList(env.toRLPEncodable, receivingAddr.toRLPEncodable, startGas)
      }
    }

    implicit class ContextDec(val bytes: Array[Byte]) extends AnyVal {
      def toContext: Context = rawDecode(bytes) match {
        case RLPList(env, receivingAddr, startGas) =>
          Context(env.toExecEnv, receivingAddr.toAddress, startGas)
        case _ => throw new RuntimeException("Cannot decode Status")
      }
    }

  implicit class ContextRLPEncodableDec(val rLPEncodeable: RLPEncodeable) extends AnyVal{
    def toContext: Context = rLPEncodeable match {
      case RLPList(env, receivingAddr, startGas) =>
        Context(env.toExecEnv, receivingAddr.toAddress, startGas)
      case _ => throw RLPException("src is not an RLPValue")
    }
  }


    implicit class ExecEnvEnc(val underlyingMsg: ExecEnv) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import underlyingMsg._
        RLPList(ownerAddr.toRLPEncodable, callerAddr.toRLPEncodable, originAddr.toRLPEncodable, gasPrice.toRLPEncodable, inputData, value.toRLPEncodable, program.toRLPEncodable, blockHeader.toRLPEncodable, callDepth)
      }
    }

    implicit class ExecEnvDec(val bytes: Array[Byte]) extends AnyVal {
      def toExecEnv: ExecEnv = rawDecode(bytes) match {
        case RLPList(ownerAddr, callerAddr, originAddr, gasPrice, inputData, value, program, blockHeader, callDepth) =>
          ExecEnv(ownerAddr.toAddress, callerAddr.toAddress, originAddr.toAddress, gasPrice.toUInt256, inputData, value.toUInt256, program.toProgram, blockHeader.toBlockHeader, callDepth)
        case _ => throw new RuntimeException("Cannot decode Status")
      }
    }

  implicit class ExecEnvRLPEncodableDec(val rLPEncodeable: RLPEncodeable) extends AnyVal{
    def toExecEnv: ExecEnv = rLPEncodeable match {
      case RLPList(ownerAddr, callerAddr, originAddr, gasPrice, inputData, value, program, blockHeader, callDepth) =>
        ExecEnv(ownerAddr.toAddress, callerAddr.toAddress, originAddr.toAddress, gasPrice.toUInt256, inputData, value.toUInt256, program.toProgram, blockHeader.toBlockHeader, callDepth)
      case _ => throw RLPException("src is not an RLPValue")
    }
  }



  implicit class AddressEnc(obj: Address) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable =
      RLPValue(obj.bytes.toArray[Byte])
  }

  implicit class AddressDec(val bytes: ByteString) extends AnyVal {
    def toAddress: Address = AddressRLPEncodableDec(rawDecode(bytes.toArray)).toAddress
  }

  implicit class AddressRLPEncodableDec(val rLPEncodeable: RLPEncodeable) extends AnyVal{
    def toAddress: Address = rLPEncodeable match {
      case RLPValue(b) => Address(b)
      case _ => throw RLPException("src is not an RLPValue")
    }
  }


  implicit class ProgramEnc(obj: Program) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable =
      RLPValue(obj.code.toArray[Byte])
  }

  implicit class ProgramDec(val bytes: ByteString) extends AnyVal {
    def toProgram: Program = ProgramRLPEncodableDec(rawDecode(bytes.toArray)).toProgram
  }

  implicit class ProgramRLPEncodableDec(val rLPEncodeable: RLPEncodeable) extends AnyVal{
    def toProgram: Program = rLPEncodeable match {
      case RLPValue(b) => Program(ByteString(b))
      case _ => throw RLPException("src is not an RLPValue")
    }
  }


  case class Context(
                      env: ExecEnv,
                      receivingAddr: Address,
                      startGas: BigInt)
}