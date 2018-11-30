package io.iohk.ethereum.blockchain.sync.fast

import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.blockchain.sync.fast.FastSync.{ EvmCodeHash, StateMptNodeHash }
import org.scalatest.{ FlatSpec, Matchers }

class FastSyncStateSpec extends FlatSpec with Matchers {

  import Fixtures.Blocks.ValidBlock._

  val number: BigInt = header.number

  "SyncState" should "prepend mpt nodes when enqueueing them" in {
    val currentMptNodes = toStateMptNodeHash("1", "2", "3")
    val currentNonMptNodes = toEvmCodeHash("a", "b", "c")
    val syncState = FastSyncState(
      targetBlock = header,
      pendingMptNodes = currentMptNodes,
      pendingNonMptNodes = currentNonMptNodes
    )

    val newMptNodes = toStateMptNodeHash("4", "5", "6")
    val newNonMptNodes = toEvmCodeHash("d", "e", "f")
    val resultingSyncState = syncState.addPendingNodes(newMptNodes).addPendingNodes(newNonMptNodes)

    resultingSyncState.pendingMptNodes shouldEqual newMptNodes ++ currentMptNodes
    resultingSyncState.pendingNonMptNodes shouldEqual newNonMptNodes ++ currentNonMptNodes
  }

  it should "enqueue block bodies when new are added" in {
    val syncState = FastSyncState(targetBlock = header)

    syncState.blockBodiesQueue shouldBe Nil
    val newSyncState = syncState.enqueueBlockBodies(toByteString("a", "b"))
    newSyncState.blockBodiesQueue.size shouldBe 2
  }

  it should "not enqueue block bodies when empty seq is added" in {
    val syncState = FastSyncState(targetBlock = header)

    syncState.blockBodiesQueue shouldBe Nil
    val newSyncState = syncState.enqueueBlockBodies(Nil)
    newSyncState.blockBodiesQueue shouldBe Nil
  }

  it should "enqueue receipts when new are added" in {
    val syncState = FastSyncState(targetBlock = header)

    syncState.receiptsQueue shouldBe Nil
    val newSyncState = syncState.enqueueReceipts(toByteString("a", "b"))
    newSyncState.receiptsQueue.size shouldBe 2
  }

  it should "not enqueue receipts when empty seq is added" in {
    val syncState = FastSyncState(targetBlock = header)

    syncState.receiptsQueue shouldBe Nil
    val newSyncState = syncState.enqueueReceipts(Nil)
    newSyncState.receiptsQueue shouldBe Nil
  }

  it should "set new best block number" in {
    val currentBest = 8
    val newBest = currentBest + 1

    val syncState = FastSyncState(targetBlock = header, bestBlockHeaderNumber = currentBest)
    syncState.bestBlockHeaderNumber shouldBe currentBest
    val newSyncState = syncState.setBestBlockNumber(newBest)
    newSyncState.bestBlockHeaderNumber shouldBe newBest
  }

  it should "update next block to validate when it's greater than best block" in {
    val syncState = FastSyncState(
      targetBlock = header,
      nextBlockToFullyValidate = 3,
      bestBlockHeaderNumber = number + 2
    )

    val newSyncState = syncState.updateNextBlockToValidate(header, 1, 1)

    newSyncState.nextBlockToFullyValidate shouldBe number + 1
  }

  it should "update next block to validate when it's smaller than best block" in {
    val syncState = FastSyncState(
      targetBlock = header,
      nextBlockToFullyValidate = 3,
      bestBlockHeaderNumber = number + 2
    )

    val newSyncState = syncState.updateNextBlockToValidate(header, 0, 10)

    newSyncState.nextBlockToFullyValidate should not be number
  }

  it should "discard updated blocks" in {
    val syncState = FastSyncState(
      targetBlock = header,
      blockBodiesQueue = toByteString("a"),
      receiptsQueue = toByteString("1")
    )

    val newSyncState = syncState.updateDiscardedBlocks(header, 0)

    newSyncState.blockBodiesQueue shouldBe Nil
    newSyncState.receiptsQueue shouldBe Nil
    newSyncState.bestBlockHeaderNumber shouldBe (number - 1)
    newSyncState.nextBlockToFullyValidate shouldBe number
  }

  it should "update target block when failures occurred" in {
    val syncState = FastSyncState(
      targetBlock = header,
      safeDownloadTarget = 1,
      targetBlockUpdateFailures = 1
    )

    val newBlockTarget = header.copy(number = number + 1)
    val safeBlocks = 10

    val newSyncState = syncState.updateTargetBlock(newBlockTarget, safeBlocks, updateFailures = true)
    newSyncState.targetBlock shouldBe newBlockTarget
    newSyncState.safeDownloadTarget shouldBe newBlockTarget.number + safeBlocks
    newSyncState.targetBlockUpdateFailures shouldBe syncState.targetBlockUpdateFailures + 1
  }

  it should "update target block when failures not occurred" in {
    val syncState = FastSyncState(
      targetBlock = header,
      safeDownloadTarget = 1,
      targetBlockUpdateFailures = 1
    )

    val newBlockTarget = header.copy(number = number + 1)
    val safeBlocks = 10

    val newSyncState = syncState.updateTargetBlock(newBlockTarget, safeBlocks, updateFailures = false)
    newSyncState.targetBlock shouldBe newBlockTarget
    newSyncState.safeDownloadTarget shouldBe newBlockTarget.number + safeBlocks
    newSyncState.targetBlockUpdateFailures shouldBe syncState.targetBlockUpdateFailures
  }

  def toStateMptNodeHash(seq: String*): Seq[StateMptNodeHash] = seq.map(s => StateMptNodeHash(ByteString(s)))

  def toEvmCodeHash(seq: String*): Seq[EvmCodeHash] = seq.map(s => EvmCodeHash(ByteString(s)))

  def toByteString(seq: String*): Seq[ByteString] = seq.map(s => ByteString(s))
}

