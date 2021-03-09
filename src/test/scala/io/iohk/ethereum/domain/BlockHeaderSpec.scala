package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields._
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.{Fixtures, ObjectGenerators, rlp}
import org.bouncycastle.util.encoders.Hex
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BlockHeaderSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with ObjectGenerators {

  "Block header encoding" - {
    "without nonce should be compatible with EthereumJ blocks" in new TestSetup {
      //Expected values obtained using EthereumJ
      val obtainedBlock1EncodedWithoutNonce = Hex.toHexString(BlockHeader.getEncodedWithoutNonce(block1))
      val expectedBlock1EncodedWithoutNonce =
        "f901e6a0d882d5c210bab4cb7ef0b9f3dc2130cb680959afcd9a8f9bf83ee6f13e2f9da3a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d493479495f484419881c6e9b6de7fb3f8ad03763bd49a89a0634a2b20c9e02afdda7157afe384306c5acc4fb9c09b45dc0203c0fbb2fed0e6a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421b9010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000830f1a4c148407d85e8f8084589e0ab998d783010507846765746887676f312e372e33856c696e7578"
      assert(obtainedBlock1EncodedWithoutNonce == expectedBlock1EncodedWithoutNonce)

      val obtainedBlock2EncodedWithoutNonce = Hex.toHexString(BlockHeader.getEncodedWithoutNonce(block2))
      val expectedBlock2EncodedWithoutNonce =
        "f901e6a0677a5fb51d52321b03552e3c667f602cc489d15fc1d7824445aee6d94a9db2e7a01dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d493479495f484419881c6e9b6de7fb3f8ad03763bd49a89a0cddeeb071e2f69ad765406fb7c96c0cd42ddfc6ec54535822b564906f9e38e44a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421a056e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421b9010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000830f1869138407da55238084589e0ab898d783010507846765746887676f312e372e33856c696e7578"
      assert(obtainedBlock2EncodedWithoutNonce == expectedBlock2EncodedWithoutNonce)
    }

    "should be symmetric with decoding" in {
      forAll(blockHeaderGen) { blockHeader =>
        val encoded: Array[Byte] = blockHeader.toBytes

        val decoded = encoded.toBlockHeader

        decoded shouldBe blockHeader
      }
    }

    "should generate the expected RLP object for pre ECIP1098 headers" in {
      val preECIP1098Header = Fixtures.Blocks.ValidBlock.header.copy(extraFields = HefEmpty)

      val expectedRLPEncoded = RLPList(
        preECIP1098Header.parentHash,
        preECIP1098Header.ommersHash,
        preECIP1098Header.beneficiary,
        preECIP1098Header.stateRoot,
        preECIP1098Header.transactionsRoot,
        preECIP1098Header.receiptsRoot,
        preECIP1098Header.logsBloom,
        preECIP1098Header.difficulty,
        preECIP1098Header.number,
        preECIP1098Header.gasLimit,
        preECIP1098Header.gasUsed,
        preECIP1098Header.unixTimestamp,
        preECIP1098Header.extraData,
        preECIP1098Header.mixHash,
        preECIP1098Header.nonce
      )

      rlp.encode(expectedRLPEncoded) shouldBe (preECIP1098Header.toBytes: Array[Byte])
    }

    "should generate the expected RLP object for post ECIP1098 headers" in {
      val postECIP1098HeaderTreasuryOptOut = true
      val postECIP1098Header = Fixtures.Blocks.ValidBlock.header.copy(
        extraFields = HefPostEcip1098(postECIP1098HeaderTreasuryOptOut)
      )

      val expectedRLPEncoded = RLPList(
        postECIP1098Header.parentHash,
        postECIP1098Header.ommersHash,
        postECIP1098Header.beneficiary,
        postECIP1098Header.stateRoot,
        postECIP1098Header.transactionsRoot,
        postECIP1098Header.receiptsRoot,
        postECIP1098Header.logsBloom,
        postECIP1098Header.difficulty,
        postECIP1098Header.number,
        postECIP1098Header.gasLimit,
        postECIP1098Header.gasUsed,
        postECIP1098Header.unixTimestamp,
        postECIP1098Header.extraData,
        postECIP1098Header.mixHash,
        postECIP1098Header.nonce,
        postECIP1098HeaderTreasuryOptOut
      )

      rlp.encode(expectedRLPEncoded) shouldBe (postECIP1098Header.toBytes: Array[Byte])
    }

    "should generate the expected RLP object for post ECIP1097 headers with checkpoint" in {
      val postECIP1097HeaderTreasuryOptOut = true
      val checkpoint = Checkpoint(Nil)
      val postECIP1097Header = Fixtures.Blocks.ValidBlock.header.copy(
        extraFields = HefPostEcip1097(postECIP1097HeaderTreasuryOptOut, Some(checkpoint))
      )

      val expectedRLPEncoded = RLPList(
        postECIP1097Header.parentHash,
        postECIP1097Header.ommersHash,
        postECIP1097Header.beneficiary,
        postECIP1097Header.stateRoot,
        postECIP1097Header.transactionsRoot,
        postECIP1097Header.receiptsRoot,
        postECIP1097Header.logsBloom,
        postECIP1097Header.difficulty,
        postECIP1097Header.number,
        postECIP1097Header.gasLimit,
        postECIP1097Header.gasUsed,
        postECIP1097Header.unixTimestamp,
        postECIP1097Header.extraData,
        postECIP1097Header.mixHash,
        postECIP1097Header.nonce,
        postECIP1097HeaderTreasuryOptOut,
        Some(checkpoint): Option[Checkpoint]
      )

      rlp.encode(expectedRLPEncoded) shouldBe (postECIP1097Header.toBytes: Array[Byte])
    }

    "should generate the expected RLP object for post ECIP1097 headers without checkpoint" in {
      val postECIP1097HeaderTreasuryOptOut = true
      val postECIP1097Header = Fixtures.Blocks.ValidBlock.header.copy(
        extraFields = HefPostEcip1097(postECIP1097HeaderTreasuryOptOut, None)
      )

      val expectedRLPEncoded = RLPList(
        postECIP1097Header.parentHash,
        postECIP1097Header.ommersHash,
        postECIP1097Header.beneficiary,
        postECIP1097Header.stateRoot,
        postECIP1097Header.transactionsRoot,
        postECIP1097Header.receiptsRoot,
        postECIP1097Header.logsBloom,
        postECIP1097Header.difficulty,
        postECIP1097Header.number,
        postECIP1097Header.gasLimit,
        postECIP1097Header.gasUsed,
        postECIP1097Header.unixTimestamp,
        postECIP1097Header.extraData,
        postECIP1097Header.mixHash,
        postECIP1097Header.nonce,
        postECIP1097HeaderTreasuryOptOut, // Defined as is a postECIP1098 block
        None: Option[Checkpoint]
      )

      rlp.encode(expectedRLPEncoded) shouldBe (postECIP1097Header.toBytes: Array[Byte])
    }
  }

  trait TestSetup {
    val block1 = BlockHeader(
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

    val block2 = BlockHeader(
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

}
