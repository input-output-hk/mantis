package io.iohk.ethereum.consensus
package atomixraft

import akka.actor.{ActorRef, PoisonPill}
import io.iohk.ethereum.consensus.atomixraft.AtomixRaftMiner.Msg
import io.iohk.ethereum.utils.Ref

// a new class to avoid "reflective access of structural type member" errors ...
final class AtomixRaftMinerRef extends Ref[ActorRef] {
  private[this] def send(msg: AnyRef): Unit = foreach(_ ! msg)

  def !(msg: Msg): Unit = send(msg)

  def kill(): Unit = send(PoisonPill)
}
