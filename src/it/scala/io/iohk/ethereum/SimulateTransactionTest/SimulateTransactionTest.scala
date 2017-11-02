package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.nodebuilder.{BlockchainConfigBuilder, SyncConfigBuilder, ValidatorsBuilder}
import org.scalatest._
import org.scalatest.FreeSpec
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig, Logger, MonetaryPolicyConfig}
import io.iohk.ethereum.vm.VM
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.domain.Block.BlockDec

class  SimulateTransactionTest extends FreeSpec with Matchers with Logger {

  "Simulate transaction with throws should correctly estimate gas usage" in new ScenarioSetup {
    /*
    * Tested transaction requires gasLimit equal to 122397, but actual gas used due to refund is equal 42907.
    * Our simulateTransaction properly estimates gas usage to 42907, but actually requires at least 122397 gas to
    * make that estimation
    *
    * */

    val lastBlockGasLimit = genesisBlock.header.gasLimit

    val tx = Transaction(0, 0, lastBlockGasLimit, existingAddress, 0, sendData)
    val fakeSignature = ECDSASignature(0, 0, 0.toByte)
    val stx = SignedTransaction(tx, fakeSignature, fromAddress)

    val someRes = ledger.simulateTransaction(stx, genesisBlock.header)
    val exec = ledger.executeTransaction(stx, genesisBlock.header, worldWithAccount)

    someRes.gasUsed shouldEqual exec.gasUsed
  }
}

trait ScenarioSetup
  extends EphemBlockchainTestSetup
  with ValidatorsBuilder
  with SyncConfigBuilder
  with BlockchainConfigBuilder {

  override lazy val blockchainConfig = new BlockchainConfig{
    override val eip155BlockNumber: BigInt = 0
    override val chainId: Byte = 0x03.toByte
    override val maxCodeSize: Option[BigInt] = None
    override val eip161BlockNumber: BigInt = 0
    override val frontierBlockNumber: BigInt = 0
    override val homesteadBlockNumber: BigInt = 0
    override val eip150BlockNumber: BigInt = 0
    override val eip160BlockNumber: BigInt = 0
    override val eip106BlockNumber: BigInt = 0
    override val difficultyBombPauseBlockNumber: BigInt = 0
    override val difficultyBombContinueBlockNumber: BigInt = 0
    override val customGenesisFileOpt: Option[String] = None
    override val accountStartNonce: UInt256 = UInt256.Zero
    override val monetaryPolicyConfig: MonetaryPolicyConfig = new MonetaryPolicyConfig(0, 0, 0)
    override val daoForkConfig: Option[DaoForkConfig] = None
    val gasTieBreaker: Boolean = false
  }

  val emptyWorld = blockchain.getWorldStateProxy(-1, UInt256.Zero, None)

  val ledger = new LedgerImpl(VM, blockchain, blockchainConfig, syncConfig, validators)

  val existingAddress = Address(10)
  val existingAccount = Account(nonce = UInt256.Zero, balance = UInt256(10))


  /*
   * Failing code which mess up with gas estimation
    contract FunkyGasPattern {
      string public field;

      function SetField(string value) {
        // This check will screw gas estimation! Good, good!
        if (msg.gas < 100000) {
          throw;
        }
        field = value;
      }
    }
    * Example from https://github.com/ethereum/go-ethereum/pull/3587
    *
    *
    */
  val failingCode = ByteString(Hex.decode("60606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806323fcf32a146100495780634f28bf0e146100a0575b610000565b346100005761009e600480803590602001908201803590602001908080601f01602080910402602001604051908101604052809392919081815260200183838082843782019150505050505091905050610136565b005b34610000576100ad6101eb565b60405180806020018281038252838181518152602001915080519060200190808383600083146100fc575b8051825260208311156100fc576020820191506020810190506020830392506100d8565b505050905090810190601f1680156101285780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b620186a05a101561014657610000565b8060009080519060200190828054600181600116156101000203166002900490600052602060002090601f016020900481019282601f1061019257805160ff19168380011785556101c0565b828001600101855582156101c0579182015b828111156101bf5782518255916020019190600101906101a4565b5b5090506101e591905b808211156101e15760008160009055506001016101c9565b5090565b50505b50565b60008054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156102815780601f1061025657610100808354040283529160200191610281565b820191906000526020600020905b81548152906001019060200180831161026457829003601f168201915b5050505050815600a165627a7a7230582075b8ec2ccf191572d3bfdd93dee7d107668265f37d7918e48132b1ba16df17320029"))

  val sendData = ByteString(Hex.decode("23fcf32a0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000564756e6e6f000000000000000000000000000000000000000000000000000000"))

  val fromAddress = Address(0)
  val startAccount = Account.empty()

  val worldWithAccount =
    InMemoryWorldStateProxy.persistState(
      emptyWorld
        .saveAccount(fromAddress, startAccount)
        .saveAccount(existingAddress, existingAccount)
        .saveCode(existingAddress, failingCode)
    )

  val someGenesisBlock =
    Hex.decode("f901fcf901f7a00000000000000000000000000000000000000000000000000000000000000000a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347948888f1f195afa192cfee860698584c030f4c9db1a07dba07d6b448a186e9612e5f737d1c909dce473e53199901a302c00646d523c1a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421b90100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000008302000080832fefd8808454c98c8142a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421880102030405060708c0c0")

  val testableGasLimit: BigInt = 122397
  val block = someGenesisBlock.toBlock
  val genesisBlock = block.copy(header = block.header.copy(stateRoot = worldWithAccount.stateRootHash, gasLimit = testableGasLimit))

  blockchain.save(genesisBlock)
  blockchain.save(genesisBlock.header.hash, Nil)
  blockchain.save(genesisBlock.header.hash, genesisBlock.header.difficulty)
}