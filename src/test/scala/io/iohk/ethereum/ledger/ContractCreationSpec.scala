package io.iohk.ethereum.ledger

import akka.util.ByteString
import akka.util.ByteString.{empty ⇒ bEmpty}
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto.{generateKeyPair, kec256}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.Ledger.PR
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.utils._
import io.iohk.ethereum.vm._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
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
      internalTxs = Nil,
      error = error
    )

  "Ledger" should "return an error if new contract's code size is larger than the limit" in new TestSetup {

    override lazy val vm: VM = new MockVM()

    val longContractCode = ByteString(Array.fill(codeSizeLimit + 1)(1.toByte))
    val resultBeforeSaving = createResult(emptyWorld(), gasUsed = defaultGasLimit / 2,
      gasLimit = defaultGasLimit, gasRefund = 0, error = None, returnData = longContractCode)

    val resultAfterSaving = ledger.saveNewContract(contractAddress, resultBeforeSaving, config)
    resultAfterSaving.error shouldBe Some(OutOfGas)
  }

  it should "not return an error if new contract's code size is smaller than the limit" in new TestSetup {

    override lazy val vm: VM = new MockVM()

    val shortContractCode = ByteString(Array.fill(codeSizeLimit - 1)(1.toByte))
    val resultBeforeSaving = createResult(emptyWorld(), gasUsed = defaultGasLimit / 2,
      gasLimit = defaultGasLimit, gasRefund = 0, error = None, returnData = shortContractCode)

    val resultAfterSaving = ledger.saveNewContract(contractAddress, resultBeforeSaving, config)
    resultAfterSaving.error shouldBe None
  }

  it should "fail to execute contract creation code in case of address conflict (non-empty code)" in
  new TestSetup with ContractCreatingTx {
    val world = worldWithCreator.saveAccount(newAddress, accountNonEmptyCode)
    val result = ledger.executeTransaction(stx, blockHeader, world)

    result.vmError shouldEqual Some(InvalidOpCode(INVALID.code))
    result.worldState.getGuaranteedAccount(newAddress) shouldEqual accountNonEmptyCode
  }

  it should "fail to execute contract creation code in case of address conflict (non-zero nonce)" in
  new TestSetup with ContractCreatingTx {
    val world = worldWithCreator.saveAccount(newAddress, accountNonZeroNonce)
    val result = ledger.executeTransaction(stx, blockHeader, world)

    result.vmError shouldEqual Some(InvalidOpCode(INVALID.code))
    result.worldState.getGuaranteedAccount(newAddress) shouldEqual accountNonZeroNonce
  }

  it should "succeed in creating a contract if the account already has some balance, but zero nonce and empty code" in
  new TestSetup with ContractCreatingTx {
    val world = worldWithCreator.saveAccount(newAddress, accountNonZeroBalance)
    val result = ledger.executeTransaction(stx, blockHeader, world)

    result.vmError shouldEqual None

    val newContract = result.worldState.getGuaranteedAccount(newAddress)
    newContract.balance shouldEqual (transferValue + accountNonZeroBalance.balance)
    newContract.nonce shouldEqual accountNonZeroBalance.nonce
    newContract.codeHash should not equal Account.EmptyCodeHash

    result.worldState.getCode(newAddress) shouldEqual ByteString(STOP.code)
  }

  it should "initialise a new contract account with zero nonce before EIP-161" in
  new TestSetup with ContractCreatingTx {
    val result = ledger.executeTransaction(stx, blockHeader, worldWithCreator)

    result.vmError shouldEqual None

    val newContract = result.worldState.getGuaranteedAccount(newAddress)
    newContract.balance shouldEqual transferValue
    newContract.nonce shouldEqual UInt256.Zero
    result.worldState.getCode(newAddress) shouldEqual ByteString(STOP.code)
  }

  it should "initialise a new contract account with incremented nonce after EIP-161" in
  new TestSetup with ContractCreatingTx {
    val result = ledger.executeTransaction(stx, blockHeader, worldWithCreatorAfterEip161)

    result.vmError shouldEqual None

    val newContract = result.worldState.getGuaranteedAccount(newAddress)
    newContract.balance shouldEqual transferValue
    newContract.nonce shouldEqual UInt256.One
    result.worldState.getCode(newAddress) shouldEqual ByteString(STOP.code)
  }

  trait TestSetup extends EphemBlockchainTestSetup with SecureRandomBuilder {
    val keyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    val contractAddress = Address(kec256(keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail))

    val codeSizeLimit = 10
    val defaultGasLimit = 5000
    val config = EvmConfig.FrontierConfigBuilder(None)

    def emptyWorld(noEmptyAccounts: Boolean = false): InMemoryWorldStateProxy =
      BlockchainImpl(storagesInstance.storages).getWorldStateProxy(-1, UInt256.Zero, None, noEmptyAccounts)

    val defaultBlockchainConfig = BlockchainConfig(Config.config)

    //+ cake overrides
    override lazy val blockchainConfig = new BlockchainConfig {
      override val maxCodeSize: Option[BigInt] = Some(codeSizeLimit)

      //unused
      override val daoForkConfig: Option[DaoForkConfig] = None
      override val customGenesisFileOpt: Option[String] = defaultBlockchainConfig.customGenesisFileOpt
      override val difficultyBombContinueBlockNumber: BigInt = defaultBlockchainConfig.difficultyBombContinueBlockNumber
      override val eip161BlockNumber: BigInt = defaultBlockchainConfig.eip161BlockNumber
      override val eip160BlockNumber: BigInt = defaultBlockchainConfig.eip160BlockNumber
      override val eip150BlockNumber: BigInt = defaultBlockchainConfig.eip150BlockNumber
      override val eip155BlockNumber: BigInt = defaultBlockchainConfig.eip155BlockNumber
      override val eip106BlockNumber: BigInt = defaultBlockchainConfig.eip106BlockNumber
      override val chainId: Byte = defaultBlockchainConfig.chainId
      override val frontierBlockNumber: BigInt = defaultBlockchainConfig.frontierBlockNumber
      override val monetaryPolicyConfig: MonetaryPolicyConfig = defaultBlockchainConfig.monetaryPolicyConfig
      override val difficultyBombPauseBlockNumber: BigInt = defaultBlockchainConfig.difficultyBombPauseBlockNumber
      override val homesteadBlockNumber: BigInt = defaultBlockchainConfig.homesteadBlockNumber
      override val accountStartNonce: UInt256 = defaultBlockchainConfig.accountStartNonce
      val gasTieBreaker: Boolean = false
    }

    // Make type more specific, this is needed for the test cases
    override lazy val ledger: LedgerImpl = newLedger()
    //- cake overrides
  }

  trait ContractCreatingTx { self: TestSetup =>

    // scalastyle:off magic.number
    val blockHeader = BlockHeader(
      parentHash = bEmpty,
      ommersHash = bEmpty,
      beneficiary = bEmpty,
      stateRoot = bEmpty,
      transactionsRoot = bEmpty,
      receiptsRoot = bEmpty,
      logsBloom = bEmpty,
      difficulty = 1000000,
      number = blockchainConfig.homesteadBlockNumber + 1,
      gasLimit = 10000000,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = bEmpty,
      mixHash = bEmpty,
      nonce = bEmpty
    )

    // code returns a single STOP instruction
    val initialisingCode = Assembly(
      PUSH1, 1,
      PUSH1, 0,
      RETURN
    ).code

    val transferValue: BigInt = 100
    val tx = Transaction(0, 1, 100000, None, transferValue, initialisingCode)

    val stx = SignedTransaction.sign(tx, keyPair, None)

    val accountNonZeroNonce = Account(nonce = 1)
    val accountNonEmptyCode = Account(codeHash = ByteString("abc"))
    val accountNonZeroBalance = Account(balance = 1)

    val creatorAccount = Account(nonce = 1, balance = 1000000)
    val creatorAddr = stx.senderAddress
    val worldWithCreator = emptyWorld().saveAccount(creatorAddr, creatorAccount)
    val worldWithCreatorAfterEip161 = emptyWorld(noEmptyAccounts = true).saveAccount(creatorAddr, creatorAccount)
    val newAddress = worldWithCreator.saveAccount(creatorAddr, creatorAccount.increaseNonce()).createAddress(creatorAddr)
  }
}
