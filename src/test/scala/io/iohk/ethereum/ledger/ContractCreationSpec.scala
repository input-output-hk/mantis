package io.iohk.ethereum.ledger

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.crypto.{generateKeyPair, kec256}
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Address, TxLogEntry}
import io.iohk.ethereum.ledger.Ledger.PR
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.utils.{BlockchainConfig, Config, MonetaryPolicyConfig, VMConfig}
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

    val ledger = new LedgerImpl(new MockVM(), defaultBlockchainConfig, vmConfig)
    val resultAfterSaving = ledger.saveNewContract(contractAddress, resultBeforeSaving, config)
    resultAfterSaving.error shouldBe Some(OutOfGas)
  }

  it should "not return an error if it's size is smaller than the limit" in new TestSetup {
    val shortContractCode = ByteString(Array.fill(codeSizeLimit - 1)(1.toByte))
    val resultBeforeSaving = createResult(emptyWorld, gasUsed = defaultGasLimit / 2,
      gasLimit = defaultGasLimit, gasRefund = 0, error = None, returnData = shortContractCode)

    val ledger = new LedgerImpl(new MockVM(), defaultBlockchainConfig, vmConfig)
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

    val vmConfig = new VMConfig {
      override val maxCodeSize: Option[BigInt] = Some(codeSizeLimit)
    }
    val defaultBlockchainConfig = BlockchainConfig(Config.config)
  }
}
