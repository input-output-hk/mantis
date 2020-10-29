package io.iohk.ethereum.blockchain.sync
import io.iohk.ethereum.domain.Block

object SyncProtocol {
  sealed trait SyncProtocolMsg
  case object Start extends SyncProtocolMsg
  case object GetStatus extends SyncProtocolMsg
  case class MinedBlock(block: Block) extends SyncProtocolMsg

  sealed trait Status {
    def syncing: Boolean = this match {
      case Status.Syncing(_, _, _) => true
      case Status.NotSyncing => false
      case Status.SyncDone => false
    }

    def notSyncing: Boolean = !syncing
  }
  object Status {
    case class Progress(current: BigInt, target: BigInt) {
      val isEmpty: Boolean = current == 0 && target == 0
      val nonEmpty = !isEmpty
    }
    object Progress {
      val empty = Progress(0, 0)
    }
    case class Syncing(
        startingBlockNumber: BigInt,
        blocksProgress: Progress,
        stateNodesProgress: Option[Progress] // relevant only in fast sync, but is required by RPC spec
    ) extends Status

    case object NotSyncing extends Status
    case object SyncDone extends Status
  }
}
