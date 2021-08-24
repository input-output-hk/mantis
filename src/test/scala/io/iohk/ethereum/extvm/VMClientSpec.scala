package io.iohk.ethereum.extvm

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalapb.GeneratedMessageCompanion

import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.extvm.msg.CallContext.Config
import io.iohk.ethereum.extvm.msg.CallResult
import io.iohk.ethereum.extvm.msg.VMQuery
import io.iohk.ethereum.utils.ForkBlockNumbers
import io.iohk.ethereum.utils.VmConfig
import io.iohk.ethereum.vm._
import io.iohk.ethereum.vm.utils.MockVmInput

class VMClientSpec extends AnyFlatSpec with Matchers with MockFactory {

  import io.iohk.ethereum.Fixtures.Blocks._
  import Implicits._

  "VMClient" should "handle call context and result" in new TestSetup {
    val programContext =
      ProgramContext[MockWorldState, MockStorage](tx, blockHeader, senderAddress, emptyWorld, evmConfig)

    val expectedBlockHeader = msg.BlockHeader(
      beneficiary = blockHeader.beneficiary,
      difficulty = blockHeader.difficulty,
      number = blockHeader.number,
      gasLimit = blockHeader.gasLimit,
      unixTimestamp = blockHeader.unixTimestamp
    )

    val expectedCallContextMsg = msg.CallContext(
      callerAddr = programContext.callerAddr,
      recipientAddr = programContext.recipientAddr.map(_.bytes).getOrElse(ByteString.empty): ByteString,
      inputData = programContext.inputData,
      callValue = programContext.value,
      gasPrice = programContext.gasPrice,
      gasProvided = programContext.startGas,
      blockHeader = Some(expectedBlockHeader),
      config = Config.Empty
    )

    inSequence {
      (messageHandler.sendMessage _).expects(expectedCallContextMsg)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.VMQuery])).expects(*).returns(resultQueryMsg)
    }

    val result = vmClient.run(programContext)

    result.error shouldBe None
    result.returnData shouldBe ByteString("0011")
    result.gasRemaining shouldBe 99
    result.gasRefund shouldBe 120
  }

  it should "handle account query" in new TestSetup {
    val testQueryAccountAddr = Address("0x129982FF")
    val testQueryAccount = Account(nonce = 11, balance = 99999999)

    val world = emptyWorld.saveAccount(testQueryAccountAddr, testQueryAccount)
    val programContext = ProgramContext[MockWorldState, MockStorage](tx, blockHeader, senderAddress, world, evmConfig)

    val getAccountMsg = msg.GetAccount(testQueryAccountAddr.bytes)
    val accountQueryMsg = msg.VMQuery(query = msg.VMQuery.Query.GetAccount(getAccountMsg))

    val expectedAccountResponseMsg = msg.Account(
      nonce = ByteString(testQueryAccount.nonce.toBigInt.toByteArray),
      balance = ByteString(testQueryAccount.balance.toBigInt.toByteArray),
      codeEmpty = true
    )

    inSequence {
      (messageHandler.sendMessage(_: msg.CallContext)).expects(*)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.VMQuery])).expects(*).returns(accountQueryMsg)
      (messageHandler.sendMessage _).expects(expectedAccountResponseMsg)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.VMQuery])).expects(*).returns(resultQueryMsg)
    }

    val result = vmClient.run(programContext)
    result.error shouldBe None
  }

  it should "handle storage query" in new TestSetup {
    val testStorageAddr = Address("0x99999999444444ffcc")
    val testStorageOffset = BigInt(123)
    val testStorageValue = BigInt(5918918239L)

    val world = emptyWorld.saveStorage(testStorageAddr, MockStorage().store(testStorageOffset, testStorageValue))
    val programContext = ProgramContext[MockWorldState, MockStorage](tx, blockHeader, senderAddress, world, evmConfig)

    val getStorageDataMsg = msg.GetStorageData(testStorageAddr, testStorageOffset)
    val storageQueryMsg = msg.VMQuery(query = msg.VMQuery.Query.GetStorageData(getStorageDataMsg))

    val expectedStorageDataResponseMsg = msg.StorageData(ByteString(testStorageValue.toByteArray))

    inSequence {
      (messageHandler.sendMessage(_: msg.CallContext)).expects(*)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.VMQuery])).expects(*).returns(storageQueryMsg)
      (messageHandler.sendMessage _).expects(expectedStorageDataResponseMsg)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.VMQuery])).expects(*).returns(resultQueryMsg)
    }

    val result = vmClient.run(programContext)
    result.error shouldBe None
  }

  it should "handle code query" in new TestSetup {
    val testCodeAddr = Address("0x1234")
    val testCodeValue = ByteString(Hex.decode("11223344991191919191919129129facefc122"))

    val world = emptyWorld.saveCode(testCodeAddr, testCodeValue)
    val programContext = ProgramContext[MockWorldState, MockStorage](tx, blockHeader, senderAddress, world, evmConfig)

    val getCodeMsg = msg.GetCode(testCodeAddr)
    val getCodeQueryMsg = msg.VMQuery(query = msg.VMQuery.Query.GetCode(getCodeMsg))

    val expectedCodeResponseMsg = msg.Code(testCodeValue)

    inSequence {
      (messageHandler.sendMessage(_: msg.CallContext)).expects(*)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.VMQuery])).expects(*).returns(getCodeQueryMsg)
      (messageHandler.sendMessage _).expects(expectedCodeResponseMsg)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.VMQuery])).expects(*).returns(resultQueryMsg)
    }

    val result = vmClient.run(programContext)
    result.error shouldBe None
  }

  it should "handle blockhash query" in new TestSetup {
    val testNumber = 87

    val world = emptyWorld.copy(numberOfHashes = 100)
    val programContext = ProgramContext[MockWorldState, MockStorage](tx, blockHeader, senderAddress, world, evmConfig)

    val getBlockhashMsg = msg.GetBlockhash(testNumber)
    val getBlockhashQueryMsg = msg.VMQuery(query = msg.VMQuery.Query.GetBlockhash(getBlockhashMsg))

    val expectedBlockhashResponseMsg = msg.Blockhash(world.getBlockHash(UInt256(testNumber)).get)

    inSequence {
      (messageHandler.sendMessage(_: msg.CallContext)).expects(*)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.VMQuery])).expects(*).returns(getBlockhashQueryMsg)
      (messageHandler.sendMessage _).expects(expectedBlockhashResponseMsg)
      (messageHandler.awaitMessage(_: GeneratedMessageCompanion[msg.VMQuery])).expects(*).returns(resultQueryMsg)
    }

    val result = vmClient.run(programContext)
    result.error shouldBe None
  }

  it should "send hello msg" in new TestSetup {
    val blockchainConfig = io.iohk.ethereum.utils.Config.blockchains.blockchainConfig
    val forkBlockNumbers: ForkBlockNumbers = blockchainConfig.forkBlockNumbers
    val expectedEthereumConfig = msg.EthereumConfig(
      frontierBlockNumber = forkBlockNumbers.frontierBlockNumber,
      homesteadBlockNumber = forkBlockNumbers.homesteadBlockNumber,
      eip150BlockNumber = forkBlockNumbers.eip150BlockNumber,
      eip160BlockNumber = forkBlockNumbers.eip160BlockNumber,
      eip161BlockNumber = forkBlockNumbers.eip161BlockNumber,
      maxCodeSize = blockchainConfig.maxCodeSize.get,
      accountStartNonce = blockchainConfig.accountStartNonce
    )
    val expectedHelloConfigMsg = msg.Hello.Config.EthereumConfig(expectedEthereumConfig)
    val expectedHelloMsg = msg.Hello(version = "testVersion", config = expectedHelloConfigMsg)
    (messageHandler.sendMessage _).expects(expectedHelloMsg)
    vmClient.sendHello("testVersion", blockchainConfig)
  }

  trait TestSetup {
    val blockHeader = Block3125369.header

    val emptyWorld: MockWorldState = MockWorldState()

    val blockchainConfigForEvm: BlockchainConfigForEvm = BlockchainConfigForEvm(
      frontierBlockNumber = 0,
      homesteadBlockNumber = 0,
      eip150BlockNumber = 0,
      eip160BlockNumber = 0,
      eip161BlockNumber = 0,
      byzantiumBlockNumber = 0,
      constantinopleBlockNumber = 0,
      istanbulBlockNumber = 0,
      maxCodeSize = None,
      accountStartNonce = 0,
      atlantisBlockNumber = 0,
      aghartaBlockNumber = 0,
      petersburgBlockNumber = 0,
      phoenixBlockNumber = 0,
      magnetoBlockNumber = 0,
      berlinBlockNumber = 0,
      chainId = 0x3d.toByte
    )
    val evmConfig: EvmConfig = EvmConfig.FrontierConfigBuilder(blockchainConfigForEvm)

    val senderAddress: Address = Address("0x01")
    val tx: SignedTransaction = MockVmInput.transaction(senderAddress, ByteString(""), 10, 123, 456)

    val callResultMsg: CallResult = msg.CallResult(
      returnData = ByteString("0011"),
      returnCode = ByteString(""),
      gasRemaining = ByteString(BigInt(99).toByteArray),
      gasRefund = ByteString(BigInt(120).toByteArray),
      error = false,
      modifiedAccounts = Nil
    )

    val resultQueryMsg: VMQuery = msg.VMQuery(query = msg.VMQuery.Query.CallResult(callResultMsg))

    val messageHandler: MessageHandler = mock[MessageHandler]

    val externalVmConfig: VmConfig.ExternalConfig = VmConfig.ExternalConfig("mantis", None, "127.0.0.1", 0)
    val vmClient = new VMClient(externalVmConfig, messageHandler, testMode = false)
  }

}
