package io.iohk.ethereum.consensus.atomixraft.difficulty

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.ScenarioSetup
import io.iohk.ethereum.consensus.atomixraft.validators.AtomixRaftBlockHeaderValidator
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain.{ Block, BlockHeader }
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }
import org.bouncycastle.util.encoders.Hex

// scalastyle:off magic.number
class DifficultySpec extends FlatSpec with Matchers with PropertyChecks {
  private val blockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("d882d5c210bab4cb7ef0b9f3dc2130cb680959afcd9a8f9bf83ee6f13e2f9da3")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("95f484419881c6e9b6de7fb3f8ad03763bd49a89")),
    stateRoot = ByteString(Hex.decode("634a2b20c9e02afdda7157afe384306c5acc4fb9c09b45dc0203c0fbb2fed0e6")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = 1, // this must be equal to the result of whatever calculation atomix-raft applies
    number = 20,
    gasLimit = 131620495,
    gasUsed = 0,
    unixTimestamp = 1486752441,
    extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
    mixHash = ByteString(Hex.decode("6bc729364c9b682cfa923ba9480367ebdfa2a9bca2a652fe975e8d5958f696dd")),
    nonce = ByteString(Hex.decode("797a8f3a494f937b"))
  )

  private val parentHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("677a5fb51d52321b03552e3c667f602cc489d15fc1d7824445aee6d94a9db2e7")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("95f484419881c6e9b6de7fb3f8ad03763bd49a89")),
    stateRoot = ByteString(Hex.decode("cddeeb071e2f69ad765406fb7c96c0cd42ddfc6ec54535822b564906f9e38e44")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = 0, // does not matter for parent
    number = 19,
    gasLimit = 131749155,
    gasUsed = 0,
    unixTimestamp = 1486752440,
    extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
    mixHash = ByteString(Hex.decode("7f9ac1ddeafff0f926ed9887b8cf7d50c3f919d902e618b957022c46c8b404a6")),
    nonce = ByteString(Hex.decode("3fc7bc671f7cee70"))
  )

  private val parentBlock = Block(parentHeader, BlockBody.empty)

  it should "validate difficulty for AtomixRaft block header" in new ScenarioSetup {
    val blockHeaderValidator = new AtomixRaftBlockHeaderValidator(blockchainConfig)
    val calculator = AtomixRaftDifficulty

    val blockNumber: BigInt = parentHeader.number + 1
    val blockTimestamp: Long = parentHeader.unixTimestamp + 6

    val difficulty = calculator.calculateDifficulty(blockNumber, blockTimestamp, parentBlock.header)
    val result = blockHeaderValidator.validate(blockHeader, parentBlock.header)

    result shouldBe Right(BlockHeaderValid)
    difficulty shouldBe 1
  }
}
