package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

//FIXME: Handle known blocks to peers
class BlockBroadcastActor(
                           peerActor: ActorRef,
                           peerManagerActor: ActorRef
                         ) extends Actor with ActorLogging {

  import BlockBroadcastActor._

  def receive: Receive = {
    //TODO: Be able to process NewBlockHashes
    case StartBlockBroadcast =>
      peerActor ! PeerActor.Subscribe(Set(NewBlock.code))
      context become processNewBlocks(Seq(), Seq())
  }

  def processNewBlocks(unprocessedNewBlocks: Seq[(Block, BigInt)], toBroadcastBlocks: Seq[(Block, BigInt)]): Receive = {

    case PeerActor.MessageReceived(m: NewBlock) =>
      log.info("Got NewBlock message {}", m)
      self ! ProcessNewBlocks
      context become processNewBlocks(
        unprocessedNewBlocks = unprocessedNewBlocks :+ (Block(m.blockHeader, m.blockBody) -> m.totalDifficulty),
        toBroadcastBlocks = toBroadcastBlocks
      )

    case ProcessNewBlocks if unprocessedNewBlocks.nonEmpty =>
      log.info("Processing new block message")
      val (blockToProcess, blockTd) = unprocessedNewBlocks.head

      //TODO: Check that we haven't yet received the block and that it is not on the blockchain (?)

      //TODO: Validate block header [EC-78]

      //FIXME: Import block to blockchain

      peerManagerActor ! GetPeers
      if(unprocessedNewBlocks.tail.nonEmpty) self ! ProcessNewBlocks
      context become processNewBlocks(unprocessedNewBlocks.tail, toBroadcastBlocks :+ (blockToProcess -> blockTd))

    case peers: Map[String, Peer] if toBroadcastBlocks.nonEmpty =>
      //TODO: Decide block propagation algorithm (for now we send block to every peer)
      peers.values.foreach{ peer =>
        toBroadcastBlocks.foreach{ case (b, td) =>
          val newBlockMsg = NewBlock(b.blockHeader, b.blockBody, td)
          log.info("Sending NewBlockMessage {} to {}", newBlockMsg, peer.id)
          peer.ref ! PeerActor.SendMessage(newBlockMsg)
        }
      }
      context become processNewBlocks(unprocessedNewBlocks, Seq())

    case other => //Nothing
      log.info("Received {}")
  }
}

object BlockBroadcastActor {
  def props(peerActor: ActorRef, peerManagerActor: ActorRef): Props = {
    Props(new BlockBroadcastActor(peerActor, peerManagerActor))
  }

  case object StartBlockBroadcast
  case object ProcessNewBlocks
}

case class Block(blockHeader: BlockHeader, blockBody: BlockBody)