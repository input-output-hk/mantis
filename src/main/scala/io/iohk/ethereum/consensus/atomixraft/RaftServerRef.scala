package io.iohk.ethereum.consensus.atomixraft

import io.atomix.protocols.raft.RaftServer
import io.iohk.ethereum.utils.Ref

final class RaftServerRef extends Ref[RaftServer] {
  def stop(): Unit = foreach(_.shutdown())
}
