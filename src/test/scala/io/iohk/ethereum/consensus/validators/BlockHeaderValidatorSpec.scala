package io.iohk.ethereum.consensus.validators

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.ethash.difficulty.EthashDifficultyCalculator
import io.iohk.ethereum.consensus.ethash.validators.EthashBlockHeaderValidator
import io.iohk.ethereum.consensus.validators.BlockHeaderError._
import io.iohk.ethereum.consensus.validators.BlockHeaderValidator._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.utils.{BlockchainConfig, Config, DaoForkConfig}
import io.iohk.ethereum.{Fixtures, ObjectGenerators}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class BlockHeaderValidatorSpec extends FlatSpec with Matchers with PropertyChecks with ObjectGenerators {


  val ExtraDataSizeLimit = 20

  //BlockHeader member's lengths obtained from Yellow paper
  val NonceLength = 8 //64bit
  val MixHashLength = 32 //256bit

  val blockchainConfig = createBlockchainConfig()

  val blockHeaderValidator = new EthashBlockHeaderValidator(blockchainConfig)
  val difficultyCalculator = new EthashDifficultyCalculator(blockchainConfig)

  "BlockHeaderValidator" should "validate correctly formed BlockHeaders" in {
    blockHeaderValidator.validate(validBlockHeader, validBlockParent) match {
      case Right(_) => succeed
      case _ => fail
    }
  }

  it should "return a failure if created based on invalid extra data" in {
    forAll(randomSizeByteStringGen(
      MaxExtraDataSize + 1,
      MaxExtraDataSize + ExtraDataSizeLimit)
    ) { wrongExtraData =>
      val invalidBlockHeader = validBlockHeader.copy(extraData = wrongExtraData)
      assert(blockHeaderValidator.validate(invalidBlockHeader, validBlockParent) == Left(HeaderExtraDataError))
    }
  }

  it should "validate DAO block (extra data)" in {
    import Fixtures.Blocks._
    val cases = Table(
      ("Block", "Parent Block", "Supports Dao Fork", "Valid"),
      (DaoForkBlock.header, DaoParentBlock.header, false, true),
      (DaoForkBlock.header, DaoParentBlock.header, true, false),
      (ProDaoForkBlock.header, DaoParentBlock.header, true, true),
      (ProDaoForkBlock.header, DaoParentBlock.header, false, true), // We don't care for extra data if no pro dao
      (ProDaoForkBlock.header.copy(extraData = ByteString("Wrond DAO Extra")), DaoParentBlock.header, true, false),
      // We need to check extradata up to 10 blocks after
      (ProDaoBlock1920009Header, ProDaoBlock1920008Header, true, true),
      (ProDaoBlock1920009Header.copy(extraData = ByteString("Wrond DAO Extra")), ProDaoBlock1920008Header, true, false),
      (ProDaoBlock1920010Header, ProDaoBlock1920009Header, true, true)
    )

    forAll(cases) { (block, parentBlock, supportsDaoFork, valid ) =>
      val blockHeaderValidator = new EthashBlockHeaderValidator(createBlockchainConfig(supportsDaoFork))
      blockHeaderValidator.validate(block, parentBlock) match {
        case Right(_) => assert(valid)
        case Left(DaoHeaderExtraDataError) => assert(!valid)
        case _ => fail()
      }
    }
  }

  it should "return a failure if created based on invalid timestamp" in {
    forAll(longGen) { timestamp =>
      val blockHeader = validBlockHeader.copy(unixTimestamp = timestamp)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
      timestamp match {
        case t if t <= validBlockParent.unixTimestamp => assert(validateResult == Left(HeaderTimestampError))
        case validBlockHeader.unixTimestamp => assert(validateResult == Right(BlockHeaderValid))
        case _ => assert(validateResult == Left(HeaderDifficultyError))
      }
    }
  }

  it should "return a failure if created based on invalid difficulty" in {
    forAll(bigIntGen) { difficulty =>
      val blockHeader = validBlockHeader.copy(difficulty = difficulty)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
      if(difficulty != validBlockHeader.difficulty) assert(validateResult == Left(HeaderDifficultyError))
      else assert(validateResult == Right(BlockHeaderValid))
    }
  }

  it should "return a failure if created based on invalid gas used" in {
    forAll(bigIntGen) { gasUsed =>
      val blockHeader = validBlockHeader.copy(gasUsed = gasUsed)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
      if(gasUsed > validBlockHeader.gasLimit) assert(validateResult == Left(HeaderGasUsedError))
      else assert(validateResult == Right(BlockHeaderValid))
    }
  }

  it should "return a failure if created based on invalid negative gas used" in {
    val gasUsed = -1
    val blockHeader = validBlockHeader.copy(gasUsed = gasUsed)
    val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
    assert(validateResult == Left(HeaderGasUsedError))
  }

  it should "return a failure if created based on invalid gas limit" in {
    val LowerGasLimit = MinGasLimit.max(
      validBlockParent.gasLimit - validBlockParent.gasLimit / GasLimitBoundDivisor + 1)
    val UpperGasLimit = validBlockParent.gasLimit + validBlockParent.gasLimit / GasLimitBoundDivisor - 1

    forAll(bigIntGen) { gasLimit =>
      val blockHeader = validBlockHeader.copy(gasLimit = gasLimit)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
      if(gasLimit < LowerGasLimit || gasLimit > UpperGasLimit) {
        validateResult shouldBe a[Left[HeaderGasLimitErrorBounds, _]]
      }
      else assert(validateResult == Right(BlockHeaderValid))
    }
  }

  it should "return a failure if created with gas limit above threshold and block number >= eip106 block number" in {
    val validParent = validBlockParent.copy(gasLimit = Long.MaxValue)
    val invalidBlockHeader = validBlockHeader.copy(gasLimit = BigInt(Long.MaxValue) + 1)
    blockHeaderValidator.validate(invalidBlockHeader, validParent) shouldBe a[Left[HeaderGasLimitErrorBounds, _]]
  }

  it should "return a failure if created based on invalid number" in {
    forAll(longGen) { number =>
      val blockHeader = validBlockHeader.copy(number = number)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParent)
      if(number != validBlockParent.number + 1)
        assert(validateResult == Left(HeaderNumberError) || validateResult == Left(HeaderDifficultyError))
      else assert(validateResult == Right(BlockHeaderValid))
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
    blockHeaderValidator.validate(validBlockHeader, blockchain.getBlockHeaderByHash _) match {
      case Right(_)  => succeed
      case _ => fail
    }
  }

  it should "return a failure if the parent's header is not in storage" in new EphemBlockchainTestSetup {
    blockHeaderValidator.validate(validBlockHeader, blockchain.getBlockHeaderByHash _) match {
      case Left(HeaderParentNotFoundError) => succeed
      case _ => fail
    }
  }

  it should "properly validate a block after difficulty bomb pause" in new EphemBlockchainTestSetup {
    val res = blockHeaderValidator.validate(pausedDifficultyBombBlock, pausedDifficultyBombBlockParent)
    res shouldBe Right(BlockHeaderValid)
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

  it should "validate constant block gas limit" in new EphemBlockchainTestSetup {
    val constGasLimit = 5000000
    val constGasConfig = blockchainConfig.copy(constantBlockGasLimit = Some(constGasLimit))
    val validator = new EthashBlockHeaderValidator(constGasConfig)

    val validHeader = validBlockHeader.copy(gasLimit = constGasLimit)
    val invalidHeader = validBlockHeader.copy(gasLimit = constGasLimit - 1)

    validator.validate(validHeader, validBlockParent) shouldBe Left(HeaderPoWError) // nonce and mixhash were not adjusted
    validator.validate(invalidHeader, validBlockParent) shouldBe Left(HeaderGasLimitErrorConst(constGasLimit, invalidHeader.gasLimit))
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

  def createBlockchainConfig(supportsDaoFork: Boolean = false): BlockchainConfig = {
    import Fixtures.Blocks._
    BlockchainConfig(Config.config).copy(
      frontierBlockNumber = 0,
      homesteadBlockNumber = 1150000,
      eip106BlockNumber = 0,
      difficultyBombPauseBlockNumber = 3000000,
      difficultyBombContinueBlockNumber = 5000000,

      daoForkConfig= Some(new DaoForkConfig {
        override val blockExtraData: Option[ByteString] = if (supportsDaoFork) Some(ProDaoForkBlock.header.extraData) else None
        override val range: Int = 10
        override val drainList: Seq[Address] = Nil
        override val forkBlockHash: ByteString = if (supportsDaoFork) ProDaoForkBlock.header.hash else DaoForkBlock.header.hash
        override val forkBlockNumber: BigInt = DaoForkBlock.header.number
        override val refundContract: Option[Address] = None
      })
    )
  }

  val ProDaoBlock1920008Header = BlockHeader(
    parentHash = ByteString(Hex.decode("05c45c9671ee31736b9f37ee98faa72c89e314059ecff3257206e6ab498eb9d1")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("2a65aca4d5fc5b5c859090a6c34d164135398226")),
    stateRoot = ByteString(Hex.decode("fa8d3b3cbd37caba2faf09d5e472ae6c47a58d846751bc72306166a71d0fa4fa")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = BigInt("62230570926948"),
    number = 1920008,
    gasLimit = 4707788,
    gasUsed = 0,
    unixTimestamp = 1469021025,
    extraData = ByteString(Hex.decode("64616f2d686172642d666f726b")),
    mixHash = ByteString(Hex.decode("e73421390c1b084a9806754b238715ec333cdccc8d09b90cb6e38a9d1e247d6f")),
    nonce = ByteString(Hex.decode("c207c8381305bef2"))
  )

  val ProDaoBlock1920009Header = BlockHeader(
    parentHash = ByteString(Hex.decode("41254723e12eb736ddef151371e4c3d614233e6cad95f2d9017de2ab8b469a18")),
    ommersHash = ByteString(Hex.decode("808d06176049aecfd504197dde49f46c3dd75f1af055e417d100228162eefdd8")),
    beneficiary = ByteString(Hex.decode("ea674fdde714fd979de3edf0f56aa9716b898ec8")),
    stateRoot = ByteString(Hex.decode("49eb333152713b78d920440ef065ed7f681611e0c2e6933d657d6f4a7f1936ee")),
    transactionsRoot = ByteString(Hex.decode("a8060f1391fd4cbde4b03d83b32a1bda445578cd6ec6b7982db20c499ed3682b")),
    receiptsRoot = ByteString(Hex.decode("ab66b1986e713eaf5621059e79f04ba9c528187c1b9da969f46442c3f915c120")),
    logsBloom = ByteString(Hex.decode("00000000000000020000000000020000000000000008000000000000000000000000000000000000000000000000400000000000000000000000000000202010000000000000000000000008000000000000000000000000400000000000000000000800000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000001001000020000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000004000000000000000000000000000000010000000000000000000000000000000100000000000000000000000000000")),
    difficulty = BigInt("62230571058020"),
    number = 1920009,
    gasLimit = 4712384,
    gasUsed = 109952,
    unixTimestamp = 1469021040,
    extraData = ByteString(Hex.decode("64616f2d686172642d666f726b")),
    mixHash = ByteString(Hex.decode("5bde79f4dc5be28af2d956e748a0d6ebc1f8eb5c1397e76729269e730611cb99")),
    nonce = ByteString(Hex.decode("2b4b464c0a4da82a"))
  )

  val ProDaoBlock1920010Header = BlockHeader(
    parentHash = ByteString(Hex.decode("69d04aec94ad69d7d190d3b51d24cd42dded0c4767598a1d30480363509acbef")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("4bb96091ee9d802ed039c4d1a5f6216f90f81b01")),
    stateRoot = ByteString(Hex.decode("6ee63abee7416d3a671bcbefa01aa5d4ea427e246d548e15c5f3d9a108e738fd")),
    transactionsRoot = ByteString(Hex.decode("0c6d4a643ed081f92e384a5853f14d7f5ff5d68b65d0c90b46159584a80effe0")),
    receiptsRoot = ByteString(Hex.decode("a7d1ddb80060d4b77c07007e9a9f0b83413bd2c5de71501683ba4764982eef4b")),
    logsBloom = ByteString(Hex.decode("00000000000000020000000000020000001000000000000000000000000000000008000000000000000000000000400000000000000000000000000000202000000000000800000000000008000000000000000000000000400000000008000000000000000000000000000000000000000000000000000000000010000000000000000000000000000221000000000000000000080400000000000000011000020000000200001000000000000000000000000000000000400000000000000000000002000000000100000000000000000000000040000000000000000000000010000000000000000000000000000000000000000000000000000000000000")),
    difficulty = BigInt("62230571189092"),
    number = 1920010,
    gasLimit = 4712388,
    gasUsed = 114754,
    unixTimestamp = 1469021050,
    extraData = ByteString(Hex.decode("657468706f6f6c2e6f7267202855533129")),
    mixHash = ByteString(Hex.decode("8f86617d6422c26a89b8b349b160973ca44f90326e758f1ef669c4046741dd06")),
    nonce = ByteString(Hex.decode("c7de19e00a8c3e32"))
  )


}
