package io.iohk.ethereum.consensus.atomixraft

import io.atomix.protocols.raft.RaftServer

final class RaftServerRef extends Ref[RaftServer] {
  def stop(): Unit = run(_.shutdown())
}
