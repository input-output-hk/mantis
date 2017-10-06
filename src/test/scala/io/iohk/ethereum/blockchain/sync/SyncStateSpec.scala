package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.blockchain.sync.FastSync.{EvmCodeHash, StateMptNodeHash, SyncState}
import org.scalatest.{FlatSpec, Matchers}

class SyncStateSpec extends FlatSpec with Matchers {

  "SyncState" should "prepend mpt nodes when enqueueing them" in {
    val syncState = SyncState(
      targetBlock = Fixtures.Blocks.ValidBlock.header,
      pendingMptNodes = toStateMptNodeHash("1", "2", "3"),
      pendingNonMptNodes = toEvmCodeHash("a", "b", "c")
    )

    val resultingSyncState = syncState.addPendingNodes(toStateMptNodeHash("4", "5", "6")).addPendingNodes(toEvmCodeHash("d", "e", "f"))

    resultingSyncState.pendingMptNodes shouldEqual toStateMptNodeHash("4", "5", "6", "1", "2", "3")
    resultingSyncState.pendingNonMptNodes shouldEqual toEvmCodeHash("d", "e", "f", "a", "b", "c")
  }


  def toStateMptNodeHash(seq: String*): Seq[StateMptNodeHash] = seq.map(s => StateMptNodeHash(ByteString(s)))

  def toEvmCodeHash(seq: String*): Seq[EvmCodeHash] = seq.map(s => EvmCodeHash(ByteString(s)))
}

