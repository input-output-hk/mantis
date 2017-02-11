package io.iohk.ethereum.network.p2p.validators

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.validators.BlockHeaderError._
import org.scalatest.prop.PropertyChecks
import org.scalatest.FunSuite
import org.spongycastle.util.encoders.Hex

class BlockHeaderValidatorSpec extends FunSuite with PropertyChecks with ObjectGenerators {
  val ExtraDataSizeLimit = 20

  //BlockHeader member's lengths obtained from Yellow paper
  val NonceLength = 8 //64bit
  val MixHashLength = 32 //256bit

  test("BlockHeaderValidator should validate correctly formed BlockHeaders") {
    BlockHeaderValidator.validate(validBlockHeader, validBlockParent) match {
      case Right(validated) if validated equals validBlockHeader => succeed
      case _ => fail
    }
  }

  test("BlockHeaderValidator should return a failure if created based on invalid extra data") {
    forAll(randomSizeByteStringGen(
      BlockHeaderValidator.MaxExtraDataSize + 1,
      BlockHeaderValidator.MaxExtraDataSize + ExtraDataSizeLimit)
    ) { wrongExtraData =>
      val invalidBlockHeader = validBlockHeader.copy(extraData = wrongExtraData)
      assert(BlockHeaderValidator.validate(invalidBlockHeader, validBlockParent) == Left(HeaderExtraDataError))
    }
  }

  test("BlockHeaderValidator should return a failure if created based on invalid timestamp") {
    forAll(longGen) { timestamp =>
      val blockHeader = validBlockHeader.copy(unixTimestamp = timestamp)
      val validateResult = BlockHeaderValidator.validate(blockHeader, validBlockParent)
      timestamp match {
        case t if t <= validBlockParent.unixTimestamp => assert(validateResult == Left(HeaderTimestampError))
        case validBlockHeader.unixTimestamp => assert(validateResult == Right(blockHeader))
        case _ => assert(validateResult == Left(HeaderDifficultyError))
      }
    }
  }

  test("BlockHeader return a failure if created based on invalid difficulty") {
    forAll(bigIntGen) { difficulty =>
      val blockHeader = validBlockHeader.copy(difficulty = difficulty)
      val validateResult = BlockHeaderValidator.validate(blockHeader, validBlockParent)
      if(difficulty != validBlockHeader.difficulty) assert(validateResult == Left(HeaderDifficultyError))
      else assert(validateResult == Right(blockHeader))
    }
  }

  test("BlockHeader should return a failure if created based on invalid gas used") {
    forAll(bigIntGen) { gasUsed =>
      val blockHeader = validBlockHeader.copy(gasUsed = gasUsed)
      val validateResult = BlockHeaderValidator.validate(blockHeader, validBlockParent)
      if(gasUsed > validBlockHeader.gasLimit) assert(validateResult == Left(HeaderGasUsedError))
      else assert(validateResult == Right(blockHeader))
    }
  }

  test("BlockHeader should return a failure if created based on invalid gas limit") {
    val LowerGasLimit = BlockHeaderValidator.MinGasLimit.max(
      validBlockParent.gasLimit - validBlockParent.gasLimit / BlockHeaderValidator.GasLimitBoundDivisor + 1)
    val UpperGasLimit = validBlockParent.gasLimit + validBlockParent.gasLimit / BlockHeaderValidator.GasLimitBoundDivisor - 1

    forAll(bigIntGen) { gasLimit =>
      val blockHeader = validBlockHeader.copy(gasLimit = gasLimit)
      val validateResult = BlockHeaderValidator.validate(blockHeader, validBlockParent)
      if(gasLimit < LowerGasLimit || gasLimit > UpperGasLimit)
        assert(validateResult == Left(HeaderGasLimitError))
      else assert(validateResult == Right(blockHeader))
    }
  }

  test("BlockHeader should return a failure if created based on invalid number") {
    forAll(bigIntGen) { number =>
      val blockHeader = validBlockHeader.copy(number = number)
      val validateResult = BlockHeaderValidator.validate(blockHeader, validBlockParent)
      if(number != validBlockParent.number + 1)
        assert(validateResult == Left(HeaderNumberError) || validateResult == Left(HeaderDifficultyError))
      else assert(validateResult == Right(blockHeader))
    }
  }

  test("BlockHeader should return a failure if created based on invalid nonce/mixHash") {
    forAll(byteStringOfLengthNGen(NonceLength), byteStringOfLengthNGen(MixHashLength)) { case (nonce, mixHash) =>
      val blockHeader =validBlockHeader.copy(nonce = nonce, mixHash = mixHash)
      val validateResult = BlockHeaderValidator.validate(blockHeader, validBlockParent)
      if(nonce != validBlockHeader.nonce || mixHash != validBlockHeader.mixHash) assert(validateResult == Left(HeaderPoWError))
      else assert(validateResult == Right(blockHeader))
    }
  }

  val validBlockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("d882d5c210bab4cb7ef0b9f3dc2130cb680959afcd9a8f9bf83ee6f13e2f9da3")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("95f484419881c6e9b6de7fb3f8ad03763bd49a89")),
    stateRoot = ByteString(Hex.decode("634a2b20c9e02afdda7157afe384306c5acc4fb9c09b45dc0203c0fbb2fed0e6")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
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
    logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
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
