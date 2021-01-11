package io.iohk.ethereum.extvm

import akka.util.ByteString
import scalapb.GeneratedMessageCompanion
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.extvm.msg.{CallContext, VMQuery}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VMServerSpec extends AnyFlatSpec with Matchers with MockFactory {

  import io.iohk.ethereum.Fixtures.Blocks._
  import Implicits._

  "VMServer" should "start and await hello message" in new TestSetup {
    inSequence {
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.Hello])).expects(*).returns(helloMsg)
      (messageHandler
        .awaitMessage(_: GeneratedMessageCompanion[msg.CallContext]))
        .expects(*)
        .throwing(new RuntimeException) // connection closed
      (messageHandler.close _).expects()
    }
    vmServer.run()
    vmServer.processingThread.join()
  }

  it should "handle incoming call context msg and respond with a call result" in new TestSetup {
    val blockHeader = Block3125369.header
    val blockHeaderMsg = msg.BlockHeader(
      beneficiary = blockHeader.beneficiary,
      difficulty = blockHeader.difficulty,
      number = blockHeader.number,
      gasLimit = blockHeader.gasLimit,
      unixTimestamp = blockHeader.unixTimestamp
    )

    val callContextMsg = msg.CallContext(
      callerAddr = Address("0x1001").bytes,
      recipientAddr = Address("0x1002").bytes,
      inputData = ByteString(),
      callValue = ByteString(BigInt(10).toByteArray),
      gasPrice = ByteString(BigInt(0).toByteArray),
      gasProvided = ByteString(BigInt(1000).toByteArray),
      blockHeader = Some(blockHeaderMsg),
      config = CallContext.Config.Empty
    )

    val expectedModifiedAccount1 = msg.ModifiedAccount(
      address = Address("0x1001").bytes,
      nonce = ByteString(BigInt(0).toByteArray),
      balance = ByteString(BigInt(90).toByteArray),
      storageUpdates = Nil,
      code = ByteString()
    )

    val expectedModifiedAccount2 = msg.ModifiedAccount(
      address = Address("0x1002").bytes,
      nonce = ByteString(BigInt(0).toByteArray),
      balance = ByteString(BigInt(210).toByteArray),
      storageUpdates = Nil,
      code = ByteString()
    )

    val expectedCallResultMsg = msg.VMQuery(query =
      msg.VMQuery.Query.CallResult(
        msg.CallResult(
          returnData = ByteString(),
          returnCode = ByteString(),
          gasRemaining = ByteString(BigInt(1000).toByteArray),
          gasRefund = ByteString(BigInt(0).toByteArray),
          error = false,
          modifiedAccounts = Seq(expectedModifiedAccount1, expectedModifiedAccount2),
          deletedAccounts = Nil,
          touchedAccounts = Seq(Address("0x1001").bytes, Address("0x1002").bytes),
          logs = Nil
        )
      )
    )

    inSequence {
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.Hello])).expects(*).returns(helloMsg)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.CallContext])).expects(*).returns(callContextMsg)
      expectAccountQuery(Address("0x1001"), response = Account(0, 100))
      expectAccountQuery(Address("0x1002"), response = Account(0, 200))
      expectCodeQuery(Address("0x1002"), response = ByteString())
      expectCodeQuery(Address("0x1001"), response = ByteString())
      (messageHandler.sendMessage _).expects(expectedCallResultMsg)
      (messageHandler
        .awaitMessage(_: GeneratedMessageCompanion[msg.CallContext]))
        .expects(*)
        .throwing(new RuntimeException) // connection closed
      (messageHandler.close _).expects()
    }

    vmServer.run()
    vmServer.processingThread.join()
  }

  trait TestSetup {
    val blockchainConfig = io.iohk.ethereum.utils.Config.blockchains.blockchainConfig
    val ethereumConfig = msg.EthereumConfig(
      frontierBlockNumber = blockchainConfig.frontierBlockNumber,
      homesteadBlockNumber = blockchainConfig.homesteadBlockNumber,
      eip150BlockNumber = blockchainConfig.eip150BlockNumber,
      eip160BlockNumber = blockchainConfig.eip160BlockNumber,
      eip161BlockNumber = blockchainConfig.eip161BlockNumber,
      maxCodeSize = ByteString(),
      accountStartNonce = blockchainConfig.accountStartNonce
    )
    val ethereumConfigMsg = msg.Hello.Config.EthereumConfig(ethereumConfig)
    val helloMsg = msg.Hello(version = "2.0", config = ethereumConfigMsg)

    val messageHandler = mock[MessageHandler]
    val vmServer = new VMServer(messageHandler)

    def expectAccountQuery(address: Address, response: Account): Unit = {
      val expectedQueryMsg = msg.VMQuery(VMQuery.Query.GetAccount(msg.GetAccount(address.bytes)))
      (messageHandler.sendMessage _).expects(expectedQueryMsg)
      val accountMsg =
        msg.Account(ByteString(response.nonce.toBigInt.toByteArray), ByteString(response.balance.toBigInt.toByteArray))
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.Account])).expects(*).returns(accountMsg)
    }

    def expectCodeQuery(address: Address, response: ByteString): Unit = {
      val expectedQueryMsg = msg.VMQuery(VMQuery.Query.GetCode(msg.GetCode(address.bytes)))
      (messageHandler.sendMessage _).expects(expectedQueryMsg)
      val codeMsg = msg.Code(response)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.Code])).expects(*).returns(codeMsg)
    }
  }

}
