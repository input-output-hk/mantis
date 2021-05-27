package io.iohk.ethereum.consensus.pow.miners

import akka.actor.typed.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.CoordinatorProtocol
import io.iohk.ethereum.domain.Block

trait MinerProtocol

object MinerProtocol {
  case object StartMining extends MinerProtocol
  case object StopMining extends MinerProtocol
  final case class ProcessMining(currentBestBlock: Block, replyTo: ActorRef[CoordinatorProtocol]) extends MinerProtocol

  sealed trait MiningResult {
    def triedHashes: Int
  }
  case class MiningSuccessful(triedHashes: Int, mixHash: ByteString, nonce: ByteString) extends MiningResult
  case class MiningUnsuccessful(triedHashes: Int) extends MiningResult
}
