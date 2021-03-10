package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Block.BlockDec
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException
import io.iohk.ethereum.utils._
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StxLedgerSpec extends AnyFlatSpec with Matchers with Logger {

  "StxLedger" should "correctly estimate minimum gasLimit to run transaction which throws" in new ScenarioSetup {

    /**
      * Transaction requires gasLimit equal to 121825, but actual gas used due to refund is equal 42907.
      * Our simulateTransaction properly estimates gas usage to 42907, but requires at least 121825 gas to
      * make that simulation
      *
      * After some investigation it seems that semantics required from estimateGas is that it should return
      * minimal gas required to sendTransaction, not minimal gas used by transaction. (it is implemented that way in
      * parity and geth)
      */

    val tx = Transaction(0, 0, lastBlockGasLimit, existingAddress, 0, sendData)
    val fakeSignature = ECDSASignature(0, 0, 0.toByte)
    val stx = SignedTransaction(tx, fakeSignature)
    val stxFromAddress = SignedTransactionWithSender(stx, fromAddress)

    val simulationResult: Ledger.TxResult =
      stxLedger.simulateTransaction(stxFromAddress, genesisHeader, None)
    val executionResult: Ledger.TxResult =
      consensus.blockPreparator.executeTransaction(stx, fromAddress, genesisHeader, worldWithAccount)
    val estimationResult: BigInt =
      stxLedger.binarySearchGasEstimation(stxFromAddress, genesisHeader, None)

    // Check that gasUsed from simulation and execution are equal
    simulationResult.gasUsed shouldEqual executionResult.gasUsed

    // Check that estimation result is equal to expected minimum
    estimationResult shouldEqual minGasLimitRequiredForFailingTransaction

    // Execute transaction with gasLimit lesser by one that estimated minimum
    val errorExecResult: Ledger.TxResult = consensus.blockPreparator.executeTransaction(
      stx.copy(tx = stx.tx.copy(gasLimit = estimationResult - 1)),
      fromAddress,
      genesisHeader,
      worldWithAccount
    )

    // Check if running with gasLimit < estimatedMinimum return error
    errorExecResult.vmError shouldBe defined
  }

  it should "correctly estimate gasLimit for value transfer transaction" in new ScenarioSetup {
    val transferValue = 2

    val tx = Transaction(0, 0, lastBlockGasLimit, existingEmptyAccountAddres, transferValue, ByteString.empty)
    val fakeSignature = ECDSASignature(0, 0, 0.toByte)
    val stx = SignedTransaction(tx, fakeSignature)

    val executionResult: Ledger.TxResult =
      consensus.blockPreparator.executeTransaction(stx, fromAddress, genesisHeader, worldWithAccount)
    val estimationResult: BigInt =
      stxLedger.binarySearchGasEstimation(SignedTransactionWithSender(stx, fromAddress), genesisHeader, None)

    estimationResult shouldEqual executionResult.gasUsed
  }

  it should "correctly simulate transaction on pending block when supplied prepared world" in new ScenarioSetup {
    val transferValue = 2

    val tx = Transaction(0, 0, lastBlockGasLimit, existingEmptyAccountAddres, transferValue, ByteString.empty)
    val fakeSignature = ECDSASignature(0, 0, 0.toByte)
    val stxFromAddress = SignedTransactionWithSender(SignedTransaction(tx, fakeSignature), fromAddress)

    val newBlock: Block = genesisBlock.copy(header = block.header.copy(number = 1, parentHash = genesisHash))

    val preparedBlock: Ledger.PreparedBlock =
      consensus.blockPreparator.prepareBlock(newBlock, genesisBlock.header, None)
    val preparedWorld: InMemoryWorldStateProxy = preparedBlock.updatedWorld
    val header: BlockHeader = preparedBlock.block.header.copy(number = 1, stateRoot = preparedBlock.stateRootHash)

    /** All operations in `ledger.prepareBlock` are performed on ReadOnlyWorldStateProxy so there are no updates in
      * underlying storages, but StateRootHash returned by it `expect` this updates to be in storages.
      * It leads to MPTexception.RootNotFound
      */

    assertThrows[MPTException](stxLedger.simulateTransaction(stxFromAddress, header, None))

    /** Solution is to return this ReadOnlyWorldStateProxy from `ledger.prepareBlock` along side with preparedBlock
      * and perform simulateTransaction on this world.
      */
    val result: Ledger.TxResult =
      stxLedger.simulateTransaction(stxFromAddress, header, Some(preparedWorld))

    result.vmError shouldBe None
  }
}

// scalastyle:off magic.number line.size.limit
trait ScenarioSetup extends EphemBlockchainTestSetup {

  override lazy val blockchainConfig: BlockchainConfig = BlockchainConfig(
    eip155BlockNumber = 0,
    chainId = 0x03.toByte,
    networkId = 1,
    protocolVersion = 63,
    maxCodeSize = None,
    eip161BlockNumber = 0,
    frontierBlockNumber = 0,
    homesteadBlockNumber = 0,
    eip150BlockNumber = 0,
    eip160BlockNumber = 0,
    eip106BlockNumber = 0,
    byzantiumBlockNumber = 0,
    constantinopleBlockNumber = 0,
    istanbulBlockNumber = 0,
    difficultyBombPauseBlockNumber = 0,
    difficultyBombContinueBlockNumber = 0,
    difficultyBombRemovalBlockNumber = Long.MaxValue,
    customGenesisFileOpt = None,
    accountStartNonce = UInt256.Zero,
    monetaryPolicyConfig = MonetaryPolicyConfig(5, 0, 0, 0),
    daoForkConfig = None,
    gasTieBreaker = false,
    ethCompatibleStorage = true,
    bootstrapNodes = Set(),
    atlantisBlockNumber = 0,
    aghartaBlockNumber = 0,
    phoenixBlockNumber = 0,
    petersburgBlockNumber = 0,
    ecip1098BlockNumber = 0,
    treasuryAddress = Address(0),
    ecip1097BlockNumber = 0,
    ecip1099BlockNumber = Long.MaxValue
  )

  override lazy val stxLedger = new StxLedger(blockchain, blockchainConfig, consensus.blockPreparator)

  val emptyWorld: InMemoryWorldStateProxy =
    blockchain.getWorldStateProxy(
      -1,
      UInt256.Zero,
      ByteString(MerklePatriciaTrie.EmptyRootHash),
      noEmptyAccounts = false,
      ethCompatibleStorage = true
    )

  val existingAddress = Address(10)
  val existingAccount = Account(nonce = UInt256.Zero, balance = UInt256(10))

  val existingEmptyAccountAddres = Address(20)
  val existingEmptyAccount: Account = Account.empty()

  /** Failing code which mess up with gas estimation
    * contract FunkyGasPattern {
    *   string public field;
    *   function SetField(string value) {
    *     // This check will screw gas estimation! Good, good!
    *     if (msg.gas < 100000) {
    *       throw;
    *     }
    *     field = value;
    *   }
    * }
    *
    * @note Example from https://github.com/ethereum/go-ethereum/pull/3587
    */
  val failingCode = ByteString(
    Hex.decode(
      "60606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806323fcf32a146100495780634f28bf0e146100a0575b610000565b346100005761009e600480803590602001908201803590602001908080601f01602080910402602001604051908101604052809392919081815260200183838082843782019150505050505091905050610136565b005b34610000576100ad6101eb565b60405180806020018281038252838181518152602001915080519060200190808383600083146100fc575b8051825260208311156100fc576020820191506020810190506020830392506100d8565b505050905090810190601f1680156101285780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b620186a05a101561014657610000565b8060009080519060200190828054600181600116156101000203166002900490600052602060002090601f016020900481019282601f1061019257805160ff19168380011785556101c0565b828001600101855582156101c0579182015b828111156101bf5782518255916020019190600101906101a4565b5b5090506101e591905b808211156101e15760008160009055506001016101c9565b5090565b50505b50565b60008054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156102815780601f1061025657610100808354040283529160200191610281565b820191906000526020600020905b81548152906001019060200180831161026457829003601f168201915b5050505050815600a165627a7a7230582075b8ec2ccf191572d3bfdd93dee7d107668265f37d7918e48132b1ba16df17320029"
    )
  )

  val sendData = ByteString(
    Hex.decode(
      "23fcf32a0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000564756e6e6f000000000000000000000000000000000000000000000000000000"
    )
  )

  val fromBalance = UInt256(10)
  val fromAddress = Address(0)
  val startAccount: Account = Account.empty().increaseBalance(fromBalance)

  val worldWithAccount: InMemoryWorldStateProxy = InMemoryWorldStateProxy.persistState(
    emptyWorld
      .saveAccount(fromAddress, startAccount)
      .saveAccount(existingEmptyAccountAddres, existingEmptyAccount)
      .saveAccount(existingAddress, existingAccount)
      .saveCode(existingAddress, failingCode)
  )

  val someGenesisBlock: Array[Byte] =
    Hex.decode(
      "f901fcf901f7a00000000000000000000000000000000000000000000000000000000000000000a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347948888f1f195afa192cfee860698584c030f4c9db1a07dba07d6b448a186e9612e5f737d1c909dce473e53199901a302c00646d523c1a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421b90100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008302000080832fefd8808454c98c8142a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421880102030405060708c0c0"
    )

  val minGasLimitRequiredForFailingTransaction: BigInt = 121825

  val block: Block = someGenesisBlock.toBlock
  val genesisBlock: Block =
    block.copy(header = block.header.copy(stateRoot = worldWithAccount.stateRootHash, gasLimit = 1000000))
  val genesisHash: ByteString = genesisBlock.header.hash
  val genesisHeader: BlockHeader = genesisBlock.header
  val genesisWeight = ChainWeight.zero.increase(genesisHeader)
  val lastBlockGasLimit: BigInt = genesisBlock.header.gasLimit

  blockchain
    .storeBlock(genesisBlock)
    .and(blockchain.storeReceipts(genesisHash, Nil))
    .and(blockchain.storeChainWeight(genesisHash, genesisWeight))
    .commit()
}
