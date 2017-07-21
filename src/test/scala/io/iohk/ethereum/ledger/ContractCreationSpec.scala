package io.iohk.ethereum.ledger

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.crypto.{generateKeyPair, kec256}
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Address, TxLogEntry}
import io.iohk.ethereum.ledger.Ledger.PR
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.utils.{BlockchainConfig, Config, MonetaryPolicyConfig}
import io.iohk.ethereum.vm._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters

class ContractCreationSpec extends FlatSpec with PropertyChecks with Matchers {

  def createResult(world: InMemoryWorldStateProxy,
                   gasUsed: BigInt,
                   gasLimit: BigInt,
                   gasRefund: BigInt,
                   error: Option[ProgramError] = None,
                   returnData: ByteString = bEmpty,
                   logs: Seq[TxLogEntry] = Nil,
                   addressesToDelete: Set[Address] = Set.empty): PR =
    ProgramResult(
      returnData = returnData,
      gasRemaining = gasLimit - gasUsed,
      world = world,
      addressesToDelete = addressesToDelete,
      logs = logs,
      gasRefund = gasRefund,
      error = error
    )

  it should "return an error if it's size is larger than the limit" in new TestSetup {
    val longContractCode = ByteString(Array.fill(codeSizeLimit + 1)(1.toByte))
    val resultBeforeSaving = createResult(emptyWorld, gasUsed = defaultGasLimit / 2,
      gasLimit = defaultGasLimit, gasRefund = 0, error = None, returnData = longContractCode)

    val ledger = new LedgerImpl(new MockVM(), blockchainConfig)
    val resultAfterSaving = ledger.saveNewContract(contractAddress, resultBeforeSaving, config)
    resultAfterSaving.error shouldBe Some(OutOfGas)
  }

  it should "not return an error if it's size is smaller than the limit" in new TestSetup {
    val shortContractCode = ByteString(Array.fill(codeSizeLimit - 1)(1.toByte))
    val resultBeforeSaving = createResult(emptyWorld, gasUsed = defaultGasLimit / 2,
      gasLimit = defaultGasLimit, gasRefund = 0, error = None, returnData = shortContractCode)

    val ledger = new LedgerImpl(new MockVM(), blockchainConfig)
    val resultAfterSaving = ledger.saveNewContract(contractAddress, resultBeforeSaving, config)
    resultAfterSaving.error shouldBe None
  }

  trait TestSetup extends SecureRandomBuilder {
    val keyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    val contractAddress = Address(kec256(keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    val codeSizeLimit = 10
    val defaultGasLimit = 5000
    val config = EvmConfig.FrontierConfigBuilder(None)

    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages
    val emptyWorld = InMemoryWorldStateProxy(storagesInstance.storages, UInt256.Zero)

    val defaultBlockchainConfig = BlockchainConfig(Config.config)
    val blockchainConfig = new BlockchainConfig {
      override val maxCodeSize: Option[BigInt] = Some(codeSizeLimit)

      //unused
      override val customGenesisFileOpt: Option[String] = defaultBlockchainConfig.customGenesisFileOpt
      override val daoForkBlockNumber: BigInt = defaultBlockchainConfig.daoForkBlockNumber
      override val difficultyBombContinueBlockNumber: BigInt = defaultBlockchainConfig.difficultyBombContinueBlockNumber
      override val eip160BlockNumber: BigInt = defaultBlockchainConfig.eip160BlockNumber
      override val eip150BlockNumber: BigInt = defaultBlockchainConfig.eip150BlockNumber
      override val eip155BlockNumber: BigInt = defaultBlockchainConfig.eip155BlockNumber
      override val chainId: Byte = defaultBlockchainConfig.chainId
      override val frontierBlockNumber: BigInt = defaultBlockchainConfig.frontierBlockNumber
      override val monetaryPolicyConfig: MonetaryPolicyConfig = defaultBlockchainConfig.monetaryPolicyConfig
      override val daoForkBlockHash: ByteString = defaultBlockchainConfig.daoForkBlockHash
      override val difficultyBombPauseBlockNumber: BigInt = defaultBlockchainConfig.difficultyBombPauseBlockNumber
      override val homesteadBlockNumber: BigInt = defaultBlockchainConfig.homesteadBlockNumber
      override val accountStartNonce: UInt256 = defaultBlockchainConfig.accountStartNonce
    }
  }
}
