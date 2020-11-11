package io.iohk.ethereum.blockchain.sync.regular

import akka.event.Logging._
import akka.util.ByteString
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.utils.ByteStringUtils._

sealed abstract class ImportMessages(block: Block) {
  import ImportMessages._
  protected lazy val hash: ByteString = block.header.hash
  protected lazy val number: BigInt = block.number

  def preImport(): LogEntry
  def importedToTheTop(): LogEntry
  def enqueued(): LogEntry
  def duplicated(): LogEntry
  def orphaned(): LogEntry
  def reorganisedChain(newBranch: List[Block]): LogEntry
  def importFailed(error: String): LogEntry
  def missingStateNode(exception: MissingNodeException): LogEntry

  def messageForImportResult(importResult: BlockImportResult): LogEntry =
    importResult match {
      case BlockImportedToTop(_) => importedToTheTop()
      case BlockEnqueued => enqueued()
      case DuplicateBlock => duplicated()
      case UnknownParent => orphaned()
      case ChainReorganised(_, newBranch, _) => reorganisedChain(newBranch)
      case BlockImportFailed(error) => importFailed(error)
    }
}

object ImportMessages {
  type LogEntry = (LogLevel, String)
}

class MinedBlockImportMessages(block: Block) extends ImportMessages(block) {
  import ImportMessages._
  override def preImport(): LogEntry = (DebugLevel, s"Importing new mined block (${block.idTag})")
  override def importedToTheTop(): LogEntry =
    (DebugLevel, s"Added new mined block $number to top of the chain")
  override def enqueued(): LogEntry = (DebugLevel, s"Mined block $number was added to the queue")
  override def duplicated(): LogEntry =
    (WarningLevel, "Mined block is a duplicate, this should never happen")
  override def orphaned(): LogEntry = (WarningLevel, "Mined block has no parent on the main chain")
  override def reorganisedChain(newBranch: List[Block]): LogEntry =
    (DebugLevel, s"Addition of new mined block $number resulting in chain reorganization")
  override def importFailed(error: String): LogEntry =
    (WarningLevel, s"Failed to execute mined block because of $error")
  override def missingStateNode(exception: MissingNodeException): LogEntry =
    (ErrorLevel, s"Ignoring mined block $exception")
}

class CheckpointBlockImportMessages(block: Block) extends ImportMessages(block) {
  import ImportMessages._
  override def preImport(): LogEntry = (DebugLevel, s"Importing new checkpoint block (${block.idTag})")
  override def importedToTheTop(): LogEntry =
    (DebugLevel, s"Added new checkpoint block $number to top of the chain")
  override def enqueued(): LogEntry = (DebugLevel, s"Checkpoint block $number was added to the queue")
  override def duplicated(): LogEntry =
    (DebugLevel, "Ignoring duplicate checkpoint block")
  override def orphaned(): LogEntry =
    (ErrorLevel, "Checkpoint block has no parent. This should never happen")
  override def reorganisedChain(newBranch: List[Block]): LogEntry =
    (DebugLevel, s"Addition of new checkpoint block $number resulting in chain reorganization")
  override def importFailed(error: String): LogEntry =
    (WarningLevel, s"Failed to execute checkpoint block because of $error")
  override def missingStateNode(exception: MissingNodeException): LogEntry =
    (ErrorLevel, s"Ignoring checkpoint block: $exception")
}

class NewBlockImportMessages(block: Block, peerId: PeerId) extends ImportMessages(block) {
  import ImportMessages._
  override def preImport(): LogEntry = (DebugLevel, s"Handling NewBlock message for block (${block.idTag})")
  override def importedToTheTop(): LogEntry =
    (InfoLevel, s"Added new block $number to the top of the chain received from $peerId")
  override def enqueued(): LogEntry = (DebugLevel, s"Block $number ($hash) from $peerId added to queue")
  override def duplicated(): LogEntry =
    (DebugLevel, s"Ignoring duplicate block $number ($hash) from $peerId")
  override def orphaned(): LogEntry = (DebugLevel, s"Ignoring orphaned block $number ($hash) from $peerId")
  override def reorganisedChain(newBranch: List[Block]): LogEntry = {
    val lastHeader = newBranch.last.header
    (
      DebugLevel,
      s"Imported block $number ($hash) from $peerId " +
        s"resulting in chain reorganisation: new branch of length ${newBranch.size} with head at block " +
        s"${lastHeader.number} (${hash2string(lastHeader.hash)})"
    )
  }
  override def importFailed(error: String): LogEntry =
    (DebugLevel, s"Failed to import block ${block.idTag} from $peerId")
  override def missingStateNode(exception: MissingNodeException): LogEntry =
    (ErrorLevel, s"Ignoring broadcast block, reason: $exception")
}
