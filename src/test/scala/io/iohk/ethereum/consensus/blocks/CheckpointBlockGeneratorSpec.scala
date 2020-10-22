package io.iohk.ethereum.consensus.blocks

import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefPostEcip1097
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BloomFilter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckpointBlockGeneratorSpec extends AnyFlatSpec with Matchers {

  it should "generate a proper block with checkpoint" in new TestSetup {

    val fakeCheckpoint = Checkpoint.empty

    val timestamp = parentBlock.header.unixTimestamp + 1

    val generatedBlock = checkpointBlockGenerator.generate(parentBlock, fakeCheckpoint)

    val expectedBlock = Block(
      BlockHeader(
        parentHash = parentBlock.hash,
        ommersHash = BlockHeader.EmptyOmmers,
        beneficiary = BlockHeader.EmptyBeneficiary,
        stateRoot = parentBlock.header.stateRoot,
        transactionsRoot = BlockHeader.EmptyMpt,
        receiptsRoot = BlockHeader.EmptyMpt,
        logsBloom = BloomFilter.EmptyBloomFilter,
        difficulty = parentBlock.header.difficulty,
        number = parentBlock.number + 1,
        gasLimit = parentBlock.header.gasLimit,
        gasUsed = UInt256.Zero,
        unixTimestamp = timestamp,
        extraData = ByteString.empty,
        mixHash = ByteString.empty,
        nonce = ByteString.empty,
        extraFields = HefPostEcip1097(false, Some(fakeCheckpoint))
      ),
      BlockBody.empty
    )

    generatedBlock shouldEqual expectedBlock
  }

  trait TestSetup {
    val parentBlock = Fixtures.Blocks.ValidBlock.block

    val checkpointBlockGenerator = new CheckpointBlockGenerator()
  }
}
