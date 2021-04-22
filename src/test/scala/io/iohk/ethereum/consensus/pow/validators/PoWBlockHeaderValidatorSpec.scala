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

    validatorForKeccak.validateEvenMore(myOwnBlockHeader12455798) shouldBe Right(BlockHeaderValid)

    // to show that indeed the right validator needs to be called
//    validatorForKeccak.validateEvenMore(validKeccakBlockHeader) shouldBe Left(HeaderPoWError)
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

  val myOwnBlockHeader12455799 = BlockHeader(
    parentHash = ByteString(Hex.decode("dd8d0d41fe4a3d0d49a45b42d9603c453fde4468790dcedef14c4bc22fdc3ec2")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("df7d7e053933b5cc24372f878c90e62dadad5d42")),
    stateRoot = ByteString(Hex.decode("4381ed0a6c2711c454de68280edccb946b3c03c55eba507a082470ad68b93ab3")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(
      Hex.decode(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" * 256
      )
    ),
    difficulty = BigInt("110448456679854"),
    number = 12455799,
    gasLimit = 8015541,
    gasUsed = 0,
    unixTimestamp = 1617091303,
    extraData = ByteString(Hex.decode("7374726174756d2d65752d32")),
    mixHash = ByteString(Hex.decode("2ba8e81776c7d09abe88bb0f28ea43e787ceaa6a321bfaf22580d9c8da3e3a3c")),
    nonce = ByteString(Hex.decode("a30e33ab97762a45"))
  )

  // BE0F76
  val myOwnBlockHeader12455798 = BlockHeader(
    parentHash = ByteString(Hex.decode("d9356325fb3d3831f1cf970752d98d1918ee1f6ec5cc5336b990b80fb609bc96")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("0239da7f7d5af4cff574c507bb6ce18ddc73b875")),
    stateRoot = ByteString(Hex.decode("e739c693e5fd432f37275058b99cf92041b058871533111992b083b69fdaaeb5")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(
      Hex.decode(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      )
    ),
    difficulty = BigInt("110448456679854"),
    number = 12455798,
    gasLimit = 8007722,
    gasUsed = 0,
    unixTimestamp = 1617091293,
    extraData = ByteString(Hex.decode("e4b883e5bda9e7a59ee4bb99e9b1bc0005")),
    mixHash = ByteString(Hex.decode("a3648aa609cc61c001c0fbc47cf49cdb40d0289b49a07cde6099b6610dda58b0")),
    nonce = ByteString(Hex.decode("9dcb2623b0a033cb"))
  )

  // 0xBE0F14
  val myOwnBlockHeader12455700 = BlockHeader(
    parentHash = ByteString(Hex.decode("7316626364cd60e9cdd347a74e8682075c334ac5e985f8027d325b223410059a")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("df7d7e053933b5cc24372f878c90e62dadad5d42")),
    stateRoot = ByteString(Hex.decode("395cd5106d5106967182bf532d56bc437363ac3f56709ca16b661acb20558979")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(
      Hex.decode(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      )
    ),
    difficulty = BigInt("110883968460840"),
    number = 12455700,
    gasLimit = 8015630,
    gasUsed = 0,
    unixTimestamp = 1617089860,
    extraData = ByteString(Hex.decode("7374726174756d2d65752d31")),
    mixHash = ByteString(Hex.decode("4951db40b28e3f2dfea6f82f0459d65535e4f3b64ce157baca03a6780e98c24c")),
    nonce = ByteString(Hex.decode("16d1016e5ca2add5"))
  )

}
