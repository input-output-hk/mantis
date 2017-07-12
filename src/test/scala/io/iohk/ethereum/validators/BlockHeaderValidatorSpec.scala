package io.iohk.ethereum.validators

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain._
import io.iohk.ethereum.utils.{BlockchainConfig, Config, MonetaryPolicyConfig}
import io.iohk.ethereum.validators.BlockHeaderError._
import io.iohk.ethereum.vm.UInt256
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class BlockHeaderValidatorSpec extends FlatSpec with Matchers with PropertyChecks with ObjectGenerators {
  val ExtraDataSizeLimit = 20

  //BlockHeader member's lengths obtained from Yellow paper
  val NonceLength = 8 //64bit
  val MixHashLength = 32 //256bit

  val blockchainConfig = new BlockchainConfig {
    override val frontierBlockNumber: BigInt = 0
    override val homesteadBlockNumber: BigInt = 1150000
    override val difficultyBombPauseBlockNumber: BigInt = 3000000
    override val difficultyBombContinueBlockNumber: BigInt = 5000000

    // unused
    override val daoForkBlockNumber: BigInt = Long.MaxValue
    override val eip155BlockNumber: BigInt = Long.MaxValue
    override val eip160BlockNumber: BigInt = Long.MaxValue
    override val eip150BlockNumber: BigInt = Long.MaxValue
    override val chainId: Byte = 0x3d.toByte
    override val daoForkBlockHash: ByteString = ByteString("unused")
    override val monetaryPolicyConfig: MonetaryPolicyConfig = null
    override val daoForkBlockTotalDifficulty: BigInt = 0
    override val customGenesisFileOpt: Option[String] = None
    override val accountStartNonce: UInt256 = UInt256.Zero
  }

  val blockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig)
  val difficultyCalculator = new DifficultyCalculator(blockchainConfig)

  "BlockHeaderValidator" should "validate correctly formed BlockHeaders" in {
    blockHeaderValidator.validate(validBlockHeader, validBlockParent) match {
      case Right(validated) if validated equals validBlockHeader => succeed
      case _ => fail
    }
  }

  it should "return a failure if created based on invalid extra data" in {
    forAll(randomSizeByteStringGen(
      blockHeaderValidator.MaxExtraDataSize + 1,
      blockHeaderValidator.MaxExtraDataSize + ExtraDataSizeLimit)
    ) { wrongExtraData =>
      val invalidBlockHeader = validBlockHeader.copy(extraData = wrongExtraData)
      assert(blockHeaderValidator.validate(invalidBlockHeader, validBlockParent) == Left(HeaderExtraDataError))
    }
  }

  it should "return a failure if created based on invalid timestamp" in {
    forAll(longGen) { timestamp =>
      val blockHeader = validBlockHeader.copy(unixTimestamp = timestamp)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
      timestamp match {
        case t if t <= validBlockParent.unixTimestamp => assert(validateResult == Left(HeaderTimestampError))
        case validBlockHeader.unixTimestamp => assert(validateResult == Right(blockHeader))
        case _ => assert(validateResult == Left(HeaderDifficultyError))
      }
    }
  }

  it should "return a failure if created based on invalid difficulty" in {
    forAll(bigIntGen) { difficulty =>
      val blockHeader = validBlockHeader.copy(difficulty = difficulty)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
      if(difficulty != validBlockHeader.difficulty) assert(validateResult == Left(HeaderDifficultyError))
      else assert(validateResult == Right(blockHeader))
    }
  }

  it should "return a failure if created based on invalid gas used" in {
    forAll(bigIntGen) { gasUsed =>
      val blockHeader = validBlockHeader.copy(gasUsed = gasUsed)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
      if(gasUsed > validBlockHeader.gasLimit) assert(validateResult == Left(HeaderGasUsedError))
      else assert(validateResult == Right(blockHeader))
    }
  }

  it should "return a failure if created based on invalid gas limit" in {
    val LowerGasLimit = blockHeaderValidator.MinGasLimit.max(
      validBlockParent.gasLimit - validBlockParent.gasLimit / blockHeaderValidator.GasLimitBoundDivisor + 1)
    val UpperGasLimit = validBlockParent.gasLimit + validBlockParent.gasLimit / blockHeaderValidator.GasLimitBoundDivisor - 1

    forAll(bigIntGen) { gasLimit =>
      val blockHeader = validBlockHeader.copy(gasLimit = gasLimit)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
      if(gasLimit < LowerGasLimit || gasLimit > UpperGasLimit)
        assert(validateResult == Left(HeaderGasLimitError))
      else assert(validateResult == Right(blockHeader))
    }
  }

  it should "return a failure if created based on invalid number" in {
    forAll(bigIntGen) { number =>
      val blockHeader = validBlockHeader.copy(number = number)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
      if(number != validBlockParent.number + 1)
        assert(validateResult == Left(HeaderNumberError) || validateResult == Left(HeaderDifficultyError))
      else assert(validateResult == Right(blockHeader))
    }
  }

  it should "return a failure if created based on invalid nonce/mixHash" in {
    val invalidNonce = ByteString(Hex.decode("0b80f001ae0c017f"))
    val invalidMixHash = ByteString(Hex.decode("1f947f00807f7f7f2f7f00ff82ff00de015980607f129c77afedff4680c10171"))
    val blockHeaderWithInvalidNonce = validBlockHeader.copy(nonce = invalidNonce)
    val blockHeaderWithInvalidMixHash = validBlockHeader.copy(mixHash = invalidMixHash)
    val blockHeaderWithInvalidNonceAndMixHash = validBlockHeader.copy(nonce = invalidNonce, mixHash = invalidMixHash)

    blockHeaderValidator.validate(blockHeaderWithInvalidNonce, validBlockParent) shouldBe Left(HeaderPoWError)
    blockHeaderValidator.validate(blockHeaderWithInvalidMixHash, validBlockParent) shouldBe Left(HeaderPoWError)
    blockHeaderValidator.validate(blockHeaderWithInvalidNonceAndMixHash, validBlockParent) shouldBe Left(HeaderPoWError)
  }

  it should "validate correctly a block whose parent is in storage" in new EphemBlockchainTestSetup {
    blockchain.save(validBlockParent)
    blockHeaderValidator.validate(validBlockHeader, blockchain) match {
      case Right(validated) if validated equals validBlockHeader => succeed
      case _ => fail
    }
  }

  it should "return a failure if the parent's header is not in storage" in new EphemBlockchainTestSetup {
    blockHeaderValidator.validate(validBlockHeader, blockchain) match {
      case Left(HeaderParentNotFoundError) => succeed
      case _ => fail
    }
  }

  it should "properly validate a block after difficulty bomb pause" in new EphemBlockchainTestSetup {
    val res = blockHeaderValidator.validate(pausedDifficultyBombBlock, pausedDifficultyBombBlockParent)
    res shouldBe Right(pausedDifficultyBombBlock)
  }

  it should "properly calculate the difficulty after difficulty bomb resume" in new EphemBlockchainTestSetup {
    val parentHeader: BlockHeader = validBlockParent.copy(
      number = 5000101,
      unixTimestamp = 1513175023,
      difficulty = BigInt("22627021745803"))

    val blockNumber: BigInt = parentHeader.number + 1
    val blockTimestamp: Long = parentHeader.unixTimestamp + 6

    val difficulty: BigInt = difficultyCalculator.calculateDifficulty(blockNumber, blockTimestamp, parentHeader)
    val expected = BigInt("22638338531720")

    difficulty shouldBe expected
  }

  val pausedDifficultyBombBlock = BlockHeader(
    parentHash = ByteString(Hex.decode("77af90df2b60071da7f11060747b6590a3bc2f357da4addccb5eef7cb8c2b723")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("10807cacf99ac84b7b8f9b4077e3a11ee8880bf9")),
    stateRoot = ByteString(Hex.decode("32deebbf585e9b0d0153b96d62283e903c10fac41fc4181438e29732c490ac6e")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = BigInt("20626433633447"),
    number = 3582022,
    gasLimit = 4700036,
    gasUsed = 0,
    unixTimestamp = 1492735637,
    extraData = ByteString(Hex.decode("d58301050b8650617269747986312e31352e31826c69")),
    mixHash = ByteString(Hex.decode("7d2db22c3dfaccb1b6927f5675ec24a41991ee4bcffdc564f940a45c1fce8acb")),
    nonce = ByteString(Hex.decode("81d6a5e8029f9446"))
  )

  val pausedDifficultyBombBlockParent = BlockHeader(
    parentHash = ByteString(Hex.decode("e6e90c1ba10df710365a2ae9f899bd787416d98f19874f4cb1a62f09c3b8277d")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("4c2b4e716883a2c3f6b980b70b577e54b9441060")),
    stateRoot = ByteString(Hex.decode("0920dc025715c278dc297aa7b2d1bf5a60666d92be22d338135d13571539fad7")),
    transactionsRoot = ByteString(Hex.decode("6616c23aeb486dd47aca667814ffed831553c7322440913b95847235a4c3bb97")),
    receiptsRoot = ByteString(Hex.decode("5fa90473cd08a08fc766329651d81bb6e4ef2bb330cf90c3025927a3bafe0c57")),
    logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000080000000000000000020000000000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000008000000000000000")),
    difficulty = BigInt("20616098743527"),
    number = 3582021,
    gasLimit = 4699925,
    gasUsed = 1005896,
    unixTimestamp = 1492735634,
    extraData = ByteString(Hex.decode("d58301050c8650617269747986312e31362e30826c69")),
    mixHash = ByteString(Hex.decode("d10215664192800200eab9ca7b90f9ceb8d8428200c2b4e6aebe2191c2a52c0e")),
    nonce = ByteString(Hex.decode("83e2d9b401cdfa77"))
  )

  val validBlockHeader = BlockHeader(
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

  val validBlockParent = BlockHeader(
    parentHash = ByteString(Hex.decode("677a5fb51d52321b03552e3c667f602cc489d15fc1d7824445aee6d94a9db2e7")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("95f484419881c6e9b6de7fb3f8ad03763bd49a89")),
    stateRoot = ByteString(Hex.decode("cddeeb071e2f69ad765406fb7c96c0cd42ddfc6ec54535822b564906f9e38e44")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = BigInt("989289"),
    number = 19,
    gasLimit = 131749155,
    gasUsed = 0,
    unixTimestamp = 1486752440,
    extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
    mixHash = ByteString(Hex.decode("7f9ac1ddeafff0f926ed9887b8cf7d50c3f919d902e618b957022c46c8b404a6")),
    nonce = ByteString(Hex.decode("3fc7bc671f7cee70"))
  )

}
