package io.iohk.ethereum.consensus.validators

import akka.util.ByteString
import io.iohk.ethereum.checkpointing.CheckpointingTestHelpers
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.validators.BlockHeaderError._
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.crypto.ECDSASignatureImplicits.ECDSASignatureOrdering
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefPostEcip1097
import io.iohk.ethereum.domain._
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.ledger.BloomFilter
import io.iohk.ethereum.nodebuilder.BlockchainConfigBuilder
import io.iohk.ethereum.utils.{BlockchainConfig, ByteStringUtils}
import io.iohk.ethereum.{Fixtures, ObjectGenerators, crypto}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import ByteStringUtils.byteStringOrdering

class BlockWithCheckpointHeaderValidatorSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with ObjectGenerators
    with MockFactory
    with SecureRandomBuilder {

  it should "validate correctly formed BlockHeader with checkpoint" in new TestSetup {
    blockHeaderValidator.validate(validBlockHeaderWithCheckpoint, validBlockParentHeader) shouldBe a[Right[_, _]]
  }

  it should "return failure if nonce is not empty" in new TestSetup {
    testOfEmptyByteString(byteString => validBlockHeaderWithCheckpoint.copy(nonce = byteString), "nonce")
  }

  it should "return failure if mixHash is not empty" in new TestSetup {
    testOfEmptyByteString(byteString => validBlockHeaderWithCheckpoint.copy(mixHash = byteString), "mixHash")
  }

  it should "return failure if extraData is not empty" in new TestSetup {
    testOfEmptyByteString(byteString => validBlockHeaderWithCheckpoint.copy(extraData = byteString), "extraData")
  }

  it should "return failure if beneficiary is not an empty address" in new TestSetup {
    testOfEmptyByteString(
      byteString => validBlockHeaderWithCheckpoint.copy(beneficiary = byteString),
      "beneficiary",
      BlockHeader.EmptyBeneficiary
    )
  }

  it should "return failure if ommers is not empty" in new TestSetup {
    testOfEmptyByteString(
      byteString => validBlockHeaderWithCheckpoint.copy(ommersHash = byteString),
      "ommersHash",
      BlockHeader.EmptyOmmers
    )
  }

  it should "return failure if logsBloom is not empty" in new TestSetup {
    testOfEmptyByteString(
      byteString => validBlockHeaderWithCheckpoint.copy(logsBloom = byteString),
      "logsBloom",
      BloomFilter.EmptyBloomFilter
    )
  }

  it should "return failure if transactionsRoot is not empty" in new TestSetup {
    testOfEmptyByteString(
      byteString => validBlockHeaderWithCheckpoint.copy(transactionsRoot = byteString),
      "transactionsRoot",
      BlockHeader.EmptyMpt
    )
  }

  it should "return failure if receiptsRoot is not empty" in new TestSetup {
    testOfEmptyByteString(
      byteString => validBlockHeaderWithCheckpoint.copy(receiptsRoot = byteString),
      "receiptsRoot",
      BlockHeader.EmptyMpt
    )
  }

  it should "return failure if treasuryOptOut is not false" in new TestSetup {
    val invalidExtraFields = HefPostEcip1097(
      true,
      validBlockHeaderWithCheckpoint.checkpoint
    )
    val blockHeader = validBlockHeaderWithCheckpoint.copy(extraFields = invalidExtraFields)
    val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParentHeader)
    assert(validateResult == Left(CheckpointHeaderTreasuryOptOutError))
  }

  it should "return failure if stateRoot is not the same as parent stateRoot" in new TestSetup {
    testOfTheSameValueAsParent(
      byteString => validBlockHeaderWithCheckpoint.copy(stateRoot = byteString),
      "stateRoot",
      validBlockParentHeader.stateRoot
    )
  }

  it should "return failure if created based on invalid timestamp" in new TestSetup {
    forAll(longGen suchThat (_ != validBlockParentHeader.unixTimestamp + 1)) { timestamp =>
      val blockHeader = validBlockHeaderWithCheckpoint.copy(unixTimestamp = timestamp)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParentHeader)
      assert(validateResult == Left(HeaderTimestampError))
    }
  }

  it should "return failure if difficulty is different than parent difficulty" in new TestSetup {
    forAll(bigIntGen suchThat (_ != validBlockParentHeader.difficulty)) { difficulty =>
      val blockHeader = validBlockHeaderWithCheckpoint.copy(difficulty = difficulty)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParentHeader)
      assert(
        validateResult == Left(HeaderNotMatchParentError("difficulty has different value that similar parent field"))
      )
    }
  }

  it should "return failure if gas used is not zero" in new TestSetup {
    forAll(bigIntGen suchThat (_ != UInt256.Zero.toBigInt)) { gasUsed =>
      val blockHeader = validBlockHeaderWithCheckpoint.copy(gasUsed = gasUsed)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParentHeader)
      assert(validateResult == Left(HeaderGasUsedError))
    }
  }

  it should "return failure if gas limit is different than parent gas limit" in new TestSetup {
    forAll(bigIntGen suchThat (_ != validBlockParentHeader.gasLimit)) { gasLimit =>
      val blockHeader = validBlockHeaderWithCheckpoint.copy(gasLimit = gasLimit)
      val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParentHeader)
      assert(
        validateResult == Left(HeaderNotMatchParentError("gasLimit has different value that similar parent field"))
      )
    }
  }

  it should "return failure if created based on invalid number" in new TestSetup {
    forAll(longGen suchThat (num => num != validBlockParentHeader.number + 1 && num >= config.ecip1097BlockNumber)) {
      number =>
        val blockHeader = validBlockHeaderWithCheckpoint.copy(number = number)
        val validateResult = blockHeaderValidator.validate(blockHeader, validBlockParentHeader)
        assert(validateResult == Left(HeaderNumberError))
    }
  }

  it should "validate correctly a block whose parent is in storage" in new TestSetup {
    blockHeaderValidator.validate(validBlockHeaderWithCheckpoint, getBlockHeaderWithParent) shouldBe a[Right[_, _]]
  }

  it should "return failure if the parent's header is not in storage" in new TestSetup {
    blockHeaderValidator.validate(validBlockHeaderWithCheckpoint, getBlockHeaderWithNone) shouldBe Left(
      HeaderParentNotFoundError
    )
  }

  it should "return failure when checkpoint signatures aren't sorted lexicographically" in new TestSetup {
    val invalidBlockHeaderExtraFields = HefPostEcip1097(
      false,
      Some(Checkpoint(validCheckpoint.signatures.reverse))
    )
    val invalidBlockHeader =
      validBlockHeaderWithCheckpoint.copy(extraFields = invalidBlockHeaderExtraFields)
    blockHeaderValidator.validate(invalidBlockHeader, validBlockParentHeader) shouldBe Left(
      HeaderInvalidOrderOfCheckpointSignatures
    )
  }

  it should "return failure when checkpoint has not enough valid signatures" in new TestSetup {
    val invalidBlockHeaderExtraFields = HefPostEcip1097(
      false,
      Some(Checkpoint(Seq(validCheckpoint.signatures.head)))
    )
    val invalidBlockHeader =
      validBlockHeaderWithCheckpoint.copy(extraFields = invalidBlockHeaderExtraFields)
    blockHeaderValidator.validate(invalidBlockHeader, validBlockParentHeader) shouldBe Left(
      HeaderWrongNumberOfCheckpointSignatures(1)
    )
  }

  it should "return failure when checkpoint has enough valid signatures, but also an invalid one" in new TestSetup {
    val invalidKeys = crypto.generateKeyPair(secureRandom)
    val invalidSignatures =
      CheckpointingTestHelpers.createCheckpointSignatures(Seq(invalidKeys), validBlockParent.hash)
    val signatures = invalidSignatures ++ validCheckpoint.signatures
    val invalidBlockHeaderExtraFields = HefPostEcip1097(
      false,
      Some(Checkpoint(signatures.sorted))
    )
    val invalidBlockHeader = validBlockHeaderWithCheckpoint.copy(extraFields = invalidBlockHeaderExtraFields)
    blockHeaderValidator.validate(invalidBlockHeader, validBlockParentHeader) shouldBe Left(
      HeaderInvalidCheckpointSignatures(
        invalidSignatures
          .map(signature =>
            (signature, signature.publicKey(validBlockParentHeader.hash).map(ByteStringUtils.hash2string))
          )
      )
    )
  }

  it should "return failure when checkpoint has no signatures" in new TestSetup {
    val invalidBlockHeaderExtraFields = HefPostEcip1097(false, Some(Checkpoint(Nil)))
    val invalidBlockHeader = validBlockHeaderWithCheckpoint.copy(extraFields = invalidBlockHeaderExtraFields)
    blockHeaderValidator.validate(invalidBlockHeader, validBlockParentHeader) shouldBe Left(
      HeaderWrongNumberOfCheckpointSignatures(0)
    )
  }

  it should "return failure when checkpoint has different signatures from the same signer" in new TestSetup {
    // This is a signature generated with `keys(0)` with a random `k` parameter, therefore it's different than the one
    // obtained with `ECDSASignature.sign`.
    // Note that this test will fail if `validBlockParent` is changed. We currently cannot generate such signature
    // programmatically.
    val sameSignerSigHex =
      "7e1573bc593f289793304c50fa8068d35f8611e5c558337c72b6bcfef1dbfc884226ad305a97659fc172d347b70ea7bfca011859118efcee33f3b5e02d31c3cd1b"
    val sameSignerSig = ECDSASignature.fromBytes(ByteStringUtils.string2hash(sameSignerSigHex)).get

    val invalidCheckpoint = Checkpoint((sameSignerSig +: validCheckpoint.signatures).sorted)

    // verify that we have 2 signatures from the same signer
    import Ordering.Implicits._
    val actualSigners = invalidCheckpoint.signatures.flatMap(_.publicKey(validBlockParent.hash)).sortBy(_.toSeq)
    val duplicatedSigner = ByteString(crypto.pubKeyFromKeyPair(keys.head))
    val expectedSigners = (keys.map(kp => ByteString(crypto.pubKeyFromKeyPair(kp))) :+ duplicatedSigner).sorted
    actualSigners shouldEqual expectedSigners

    val headerWithInvalidCheckpoint = checkpointBlockGenerator
      .generate(
        validBlockParent,
        invalidCheckpoint
      )
      .header

    val expectedError = {
      val invalidSigs =
        invalidCheckpoint.signatures
          .filter(_.publicKey(validBlockParent.hash).contains(duplicatedSigner))
          .map(_ -> Some(ByteStringUtils.hash2string(duplicatedSigner)))
      Left(HeaderInvalidCheckpointSignatures(invalidSigs))
    }

    blockHeaderValidator.validate(headerWithInvalidCheckpoint, validBlockParentHeader) shouldBe expectedError
  }

  it should "return when failure when checkpoint has too many signatures" in new TestSetup {
    val invalidCheckpoint =
      validCheckpoint.copy(signatures = (validCheckpoint.signatures ++ validCheckpoint.signatures).sorted)
    val invalidBlockHeaderExtraFields = HefPostEcip1097(false, Some(invalidCheckpoint))
    val invalidBlockHeader = validBlockHeaderWithCheckpoint.copy(extraFields = invalidBlockHeaderExtraFields)

    blockHeaderValidator.validate(invalidBlockHeader, validBlockParentHeader) shouldBe Left(
      HeaderWrongNumberOfCheckpointSignatures(4)
    )
  }

  trait TestSetup extends BlockchainConfigBuilder {
    val validBlockParent = Fixtures.Blocks.ValidBlock.block
    val validBlockParentHeader = validBlockParent.header

    final val checkpointPubKeys =
      Set(
        // prv ee4fd3a153f6d66918d7a54e33b8fbafcb6a786551306d1248e62bef76e8fe52
        ByteStringUtils.string2hash(
          "f24c509e40a62c412799794fcd8ce23e1857a650e0d34e34ed8f0943176a6de9b39efb6c31791e1bd6c91ab4d3fa9c2740ff7437e2f4df1c6a0afb0b4ff3a5f2"
        ),
        // prv 50fb0844af1015ec78e2f5e59396beea2933a8fa071279e247dc616db3db8609
        ByteStringUtils.string2hash(
          "efc775224817ad9177086172cf8bc6f0cf658960544cb1f1265a1ab94e411664f889a30639f7e04f026237e1a066c87f2e5c455da2a9a94b8f98537dcff5ea9e"
        ),
        // prv 1c8a5770eb2e3858f7f0e113ece1a513d61e1528a44d6f5b70738d804a8ee85f
        ByteStringUtils.string2hash(
          "6848a3ab71918f57d3b9116b8e93c6fbc53e8a28dcd63e99c514dceee30fdd9741050fa7646bd196c9512e52f0d03097678c707996fff55587cd467801a1eee1"
        )
      )
    val config: BlockchainConfig = blockchainConfig.copy(
      ecip1097BlockNumber = validBlockParentHeader.number,
      ecip1098BlockNumber = validBlockParentHeader.number,
      eip106BlockNumber = 0,
      checkpointPubKeys = checkpointPubKeys
    )

    val keys = Seq(
      crypto.keyPairFromPrvKey(
        ByteStringUtils.string2hash("ee4fd3a153f6d66918d7a54e33b8fbafcb6a786551306d1248e62bef76e8fe52").toArray
      ),
      crypto.keyPairFromPrvKey(
        ByteStringUtils.string2hash("50fb0844af1015ec78e2f5e59396beea2933a8fa071279e247dc616db3db8609").toArray
      )
    )

    val validCheckpoint = Checkpoint(
      CheckpointingTestHelpers.createCheckpointSignatures(keys, validBlockParentHeader.hash)
    )

    def blockHeaderValidatorBuilder(config: BlockchainConfig): BlockHeaderValidatorSkeleton =
      new BlockHeaderValidatorSkeleton(config) {
        override def difficulty: DifficultyCalculator =
          (_: BigInt, _: Long, _: BlockHeader) => 0

        override def validateEvenMore(
            blockHeader: BlockHeader,
            parentHeader: BlockHeader
        ): Either[BlockHeaderError, BlockHeaderValid] = Right(BlockHeaderValid)
      }

    val blockHeaderValidator = blockHeaderValidatorBuilder(config)

    val checkpointBlockGenerator = new CheckpointBlockGenerator

    val validBlockHeaderWithCheckpoint =
      checkpointBlockGenerator
        .generate(
          validBlockParent,
          validCheckpoint
        )
        .header

    val randomSizeByteStringGenerator = randomSizeByteStringGen(0, 32)

    def getBlockHeaderByHashMock(blockHeaders: Seq[BlockHeader])(hash: ByteString): Option[BlockHeader] =
      blockHeaders.find(_.hash == hash)
    val getBlockHeaderWithParent = getBlockHeaderByHashMock(Seq(validBlockParentHeader)) _
    val getBlockHeaderWithNone = getBlockHeaderByHashMock(Nil) _

    def testOfEmptyByteString(
        invalidBlockHeaderCreator: ByteString => BlockHeader,
        fieldName: String,
        emptyValue: ByteString = ByteString.empty
    ): Assertion = {
      forAll(randomSizeByteStringGenerator suchThat (_ != emptyValue)) { byteString =>
        val invalidBlockHeader = invalidBlockHeaderCreator(byteString)
        assert(
          blockHeaderValidator
            .validate(invalidBlockHeader, validBlockParentHeader) == Left(
            HeaderFieldNotEmptyError(s"$fieldName is not empty")
          )
        )
      }
    }

    def testOfTheSameValueAsParent(
        invalidBlockHeaderCreator: ByteString => BlockHeader,
        fieldName: String,
        filteredValue: ByteString
    ): Assertion = {
      forAll(randomSizeByteStringGenerator suchThat (_ != filteredValue)) { byteString =>
        val invalidBlockHeader = invalidBlockHeaderCreator(byteString)
        assert(
          blockHeaderValidator
            .validate(invalidBlockHeader, validBlockParentHeader) == Left(
            HeaderNotMatchParentError(s"$fieldName has different value that similar parent field")
          )
        )
      }
    }

  }

}
