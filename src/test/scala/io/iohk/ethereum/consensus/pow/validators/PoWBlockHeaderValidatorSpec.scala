package io.iohk.ethereum.consensus.pow.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.pow.KeccakDataUtils
import io.iohk.ethereum.consensus.pow.KeccakDataUtils.header
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderPoWError
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.Config
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class PoWBlockHeaderValidatorSpec extends AnyFlatSpecLike with Matchers {
  import PoWBlockHeaderValidatorSpec._

  "PoWBlockHeaderValidator" should "call KeccakBlockHeaderValidator when chain is in Keccak" in {
    val keccakConfig = blockchainConfig.copy(ecip1049BlockNumber = Some(10))
    val validatorForKeccak = new PoWBlockHeaderValidator(keccakConfig)

    validatorForKeccak.validateEvenMore(validKeccakBlockHeader) shouldBe Right(BlockHeaderValid)

    // to show that indeed the right validator needs to be called
    validatorForKeccak.validateEvenMore(validEthashBlockHeader) shouldBe Left(HeaderPoWError)
  }

  it should "call EthashBlockHeaderValidator when chain is not in Keccak" in {
    val validatorForKeccak = new PoWBlockHeaderValidator(blockchainConfig)

    validatorForKeccak.validateEvenMore(validEthashBlockHeader) shouldBe Right(BlockHeaderValid)

    // to show that indeed the right validator needs to be called
    validatorForKeccak.validateEvenMore(validKeccakBlockHeader) shouldBe Left(HeaderPoWError)
  }
}

object PoWBlockHeaderValidatorSpec {
  val blockchainConfig = Config.blockchains.blockchainConfig

  val validKeccakBlockHeader = KeccakDataUtils.header.copy(
    mixHash = ByteString(Hex.decode("d033f82e170ff16640e902fad569243c39bce9e4da948ccc298c541b34cd263b")),
    nonce = ByteString(Hex.decode("f245822d3412da7f"))
  )

  val validEthashBlockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("d882d5c210bab4cb7ef0b9f3dc2130cb680959afcd9a8f9bf83ee6f13e2f9da3")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("95f484419881c6e9b6de7fb3f8ad03763bd49a89")),
    stateRoot = ByteString(Hex.decode("634a2b20c9e02afdda7157afe384306c5acc4fb9c09b45dc0203c0fbb2fed0e6")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = BigInt("989772"),
    number = 20,
    gasLimit = 131620495,
    gasUsed = 0,
    unixTimestamp = 1486752441,
    extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
    mixHash = ByteString(Hex.decode("6bc729364c9b682cfa923ba9480367ebdfa2a9bca2a652fe975e8d5958f696dd")),
    nonce = ByteString(Hex.decode("797a8f3a494f937b"))
  )
}
