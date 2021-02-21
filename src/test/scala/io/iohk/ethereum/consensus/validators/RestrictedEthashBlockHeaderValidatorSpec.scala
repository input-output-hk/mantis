package io.iohk.ethereum.consensus.validators

import akka.util.ByteString
import io.iohk.ethereum.consensus.ethash.RestrictedEthashSigner
import io.iohk.ethereum.consensus.ethash.validators.RestrictedEthashBlockHeaderValidator
import io.iohk.ethereum.consensus.validators.BlockHeaderError.{HeaderPoWError, RestrictedEthashHeaderExtraDataError}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Address, BlockHeader, UInt256}
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.{BlockchainConfig, ByteStringUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RestrictedEthashBlockHeaderValidatorSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with SecureRandomBuilder {

  "RestrictedEthashBlockHeaderValidatorSpec" should "correctly validate header if allowed list is empty" in new TestSetup {
    val blockHeaderValidator = new RestrictedEthashBlockHeaderValidator(createBlockchainConfig(Set()))
    val validationResult = blockHeaderValidator.validate(validHeader, validParent)
    assert(validationResult == Right(BlockHeaderValid))
  }

  it should "fail validation of header with too long extra data field" in new TestSetup {
    val blockHeaderValidator = new RestrictedEthashBlockHeaderValidator(createBlockchainConfig(Set()))
    val tooLongExtraData = validHeader.copy(extraData =
      ByteString.fromArrayUnsafe(new Array[Byte](blockHeaderValidator.ExtraDataMaxSize + 1))
    )
    val validationResult = blockHeaderValidator.validate(tooLongExtraData, validParent)
    assert(validationResult == Left(RestrictedEthashHeaderExtraDataError))
  }

  it should "correctly validate header with valid key" in new TestSetup {
    val blockHeaderValidator = new RestrictedEthashBlockHeaderValidator(createBlockchainConfig(Set(validKey)))
    val validationResult = blockHeaderValidator.validate(validHeader, validParent)
    assert(validationResult == Right(BlockHeaderValid))
  }

  it should "fail to validate header with invalid key" in new TestSetup {
    val allowedKey = crypto.generateKeyPair(secureRandom)
    val keyBytes = crypto.keyPairToByteStrings(allowedKey)._2

    // correct header is signed by different key that the one generated here
    val blockHeaderValidator = new RestrictedEthashBlockHeaderValidator(createBlockchainConfig(Set(keyBytes)))
    val validationResult = blockHeaderValidator.validate(validHeader, validParent)
    assert(validationResult == Left(RestrictedEthashHeaderExtraDataError))
  }

  it should "fail to validate header re-signed by valid signer" in new TestSetup {
    val allowedKey = crypto.generateKeyPair(secureRandom)
    val keyBytes = crypto.keyPairToByteStrings(allowedKey)._2

    val blockHeaderValidator = new RestrictedEthashBlockHeaderValidator(createBlockchainConfig(Set(keyBytes, validKey)))
    val headerWithoutSig = validHeader.copy(extraData = validHeader.extraData.dropRight(ECDSASignature.EncodedLength))
    val reSignedHeader = RestrictedEthashSigner.signHeader(headerWithoutSig, allowedKey)

    val validationResult = blockHeaderValidator.validate(reSignedHeader, validParent)
    assert(validationResult == Left(HeaderPoWError))
  }

  trait TestSetup {
    val validKey = ByteStringUtils.string2hash(
      "69f6b54223c0d699c91f1f649e11dc52cb05910896b80c50137cd74a54d90782b69128d3ad5a9ba8c26e338891e33a46e317a3eeaabbf62e70a6b33ec57e00e6"
    )
    def createBlockchainConfig(allowedMiners: Set[ByteString]): BlockchainConfig = {
      BlockchainConfig(
        frontierBlockNumber = 0,
        homesteadBlockNumber = 1150000,
        difficultyBombPauseBlockNumber = 3000000,
        difficultyBombContinueBlockNumber = 5000000,
        difficultyBombRemovalBlockNumber = 5900000,
        byzantiumBlockNumber = 4370000,
        constantinopleBlockNumber = 7280000,
        istanbulBlockNumber = 9069000,
        daoForkConfig = None,
        // unused
        maxCodeSize = None,
        eip155BlockNumber = Long.MaxValue,
        eip160BlockNumber = Long.MaxValue,
        eip161BlockNumber = Long.MaxValue,
        eip150BlockNumber = Long.MaxValue,
        eip106BlockNumber = 0,
        chainId = 0x3d.toByte,
        networkId = 1,
        monetaryPolicyConfig = null,
        customGenesisFileOpt = None,
        accountStartNonce = UInt256.Zero,
        bootstrapNodes = Set(),
        gasTieBreaker = false,
        ethCompatibleStorage = true,
        atlantisBlockNumber = Long.MaxValue,
        aghartaBlockNumber = Long.MaxValue,
        phoenixBlockNumber = Long.MaxValue,
        petersburgBlockNumber = Long.MaxValue,
        ecip1098BlockNumber = Long.MaxValue,
        treasuryAddress = Address(0),
        ecip1097BlockNumber = Long.MaxValue,
        checkpointPubKeys = Set.empty,
        allowedMinersPublicKeys = allowedMiners,
        ecip1099BlockNumber = Long.MaxValue
      )
    }

    /**
      * validParent and validHeader are special headers with extended extraData field and are only useful when used
      * with RestrictedEthashBlockHeaderValidator
      */
    val validParent = BlockHeader(
      parentHash = ByteStringUtils.string2hash("c12a822d0c9a1a777cd1023172ec304aca76e403355e4eb56592d299e4b86503"),
      ommersHash = ByteStringUtils.string2hash("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347"),
      beneficiary = ByteStringUtils.string2hash("0011223344556677889900112233445566778899"),
      stateRoot = ByteStringUtils.string2hash("e3a3e62598cdb02a3551f9e932ed248a741ca174c00d977a56d9bb2c6473dd34"),
      transactionsRoot =
        ByteStringUtils.string2hash("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"),
      receiptsRoot = ByteStringUtils.string2hash("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"),
      logsBloom = ByteStringUtils.string2hash("00" * 256),
      difficulty = BigInt("131520"),
      number = 10,
      gasLimit = 5030,
      gasUsed = 0,
      unixTimestamp = 1605514463,
      extraData = ByteStringUtils.string2hash(
        "6d616e746973808fc245b89183f28ac985019992f202a73c7ab600b0aefa18dcba71a8f3576129280d56f4f499e7a8a53a047e91d73d881745b7a6ac7ca9449fc2b3bb1608921c"
      ),
      mixHash = ByteStringUtils.string2hash("2db10efede75cfe87b6f378d9b03e712098e8cd3759784db56d65cc9e9911675"),
      nonce = ByteStringUtils.string2hash("a57246871d5c8bcc")
    )

    val validHeader = BlockHeader(
      parentHash = ByteStringUtils.string2hash("28aad5edd02d139bf4fcf15d04ec04c93f12e382c64983fa271a9084189b3b23"),
      ommersHash = ByteStringUtils.string2hash("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347"),
      beneficiary = ByteStringUtils.string2hash("0011223344556677889900112233445566778899"),
      stateRoot = ByteStringUtils.string2hash("a485afd5bfcef9da8df9c0fe4315e1f4bc2c96eb34920eeaddf534b807cd71e6"),
      transactionsRoot =
        ByteStringUtils.string2hash("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"),
      receiptsRoot = ByteStringUtils.string2hash("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"),
      logsBloom = ByteStringUtils.string2hash("00" * 256),
      difficulty = BigInt("131584"),
      number = 11,
      gasLimit = 5033,
      gasUsed = 0,
      unixTimestamp = 1605514466,
      extraData = ByteStringUtils.string2hash(
        "6d616e746973dccb0bbbfb07910cf745bde048bd0887d03e2ac790575b7cad36bf44d83e55877ea832719c978d2336b64c2200d0ced5777cd98e2d74d2cd5c0608c8a91067ae1b"
      ),
      mixHash = ByteStringUtils.string2hash("311575b0d0550f5c8858636621c66172c2633f0a6d6d7f7a254c5be9fcc998a5"),
      nonce = ByteStringUtils.string2hash("b841838f136f2bed")
    )
  }
}
