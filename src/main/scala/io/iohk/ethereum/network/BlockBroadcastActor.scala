package io.iohk.ethereum.network

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import io.iohk.ethereum.db.storage.{BlockBodiesStorage, BlockHeadersStorage, TotalDifficultyStorage}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

//FIXME: Handle known blocks to peers [EC-80]
//TODO: One actor per peer or only one actor for all of them?
class BlockBroadcastActor(
                           peerActor: ActorRef,
                           peerManagerActor: ActorRef,
                           storage: BlockBroadcastActor.Storage
                         ) extends Actor with ActorLogging {

  import BlockBroadcastActor._

  override def receive: Receive = idle

  def idle: Receive = {
    //TODO: Be able to process NewBlockHashes
    case StartBlockBroadcast =>
      peerActor ! PeerActor.Subscribe(Set(NewBlock.code))
      context become processNewBlockMessages(Seq(), Seq())
  }

  def processNewBlockMessages(unprocessedNewBlocks: Seq[Block], toBroadcastBlocks: Seq[Block]): Receive = {

    case PeerActor.MessageReceived(m: NewBlock) =>
      //Check that the NewBlock should be processed
      val newBlock = Block(m.blockHeader, m.blockBody)
      val blockNotProcessed = !(unprocessedNewBlocks ++ toBroadcastBlocks).contains(newBlock)
      val blockNotInStorage = storage.blockHeadersStorage.get(m.blockHeader.hash).isEmpty

      //TODO: Check if the block's number is ok (not too old and not too into the future)
      //      We need to know our current height (from blockchain)
      //      We also need to decide how to handle blocks too into the future (Geth or EthereumJ approach)

      if(blockNotProcessed && blockNotInStorage){
        log.debug("Got NewBlock message {}", m)
        self ! ProcessNewBlocks
        context become processNewBlockMessages(
          unprocessedNewBlocks = unprocessedNewBlocks :+ newBlock,
          toBroadcastBlocks = toBroadcastBlocks
        )
      }

    case ProcessNewBlocks if unprocessedNewBlocks.nonEmpty => processBlock(unprocessedNewBlocks, toBroadcastBlocks)

    case peers: Map[String, Peer] if toBroadcastBlocks.nonEmpty =>
      //TODO: Decide block propagation algorithm (for now we send block to every peer)
      toBroadcastBlocks.foreach{ b =>
        val blockTd = b.blockHeader.difficulty + storage.totalDifficultyStorage.get(b.blockHeader.parentHash)
          .getOrElse(throw new Exception("Block td is not on storage"))
        peers.values.foreach{ peer =>
          val newBlockMsg = NewBlock(b.blockHeader, b.blockBody, blockTd)
          log.debug("Sending NewBlockMessage {} to {}", newBlockMsg, peer.id)
          peer.ref ! PeerActor.SendMessage(newBlockMsg)
        }
      }
      context become processNewBlockMessages(unprocessedNewBlocks, Seq())

    case other => //Nothing
      log.debug("Received {}")
  }

  private def processBlock(unprocessedNewBlocks: Seq[Block], toBroadcastBlocks: Seq[Block]) = {
    val blockToProcess = unprocessedNewBlocks.head
    log.debug("Processing new block {}", blockToProcess)

    val block: Option[Block] = for {
      parentHeader <- storage.blockHeadersStorage.get(blockToProcess.blockHeader.parentHash)
      parentBody <- storage.blockBodiesStorage.get(blockToProcess.blockHeader.parentHash)
      //TODO: Validate block header [EC-78] (using block parent)
      //TODO: Import block to blockchain
    } yield blockToProcess

    if(unprocessedNewBlocks.tail.nonEmpty) self ! ProcessNewBlocks
    block match {
      case Some(b: Block) =>
        peerManagerActor ! GetPeers
        context become processNewBlockMessages(unprocessedNewBlocks.tail, toBroadcastBlocks :+ blockToProcess)
      case None =>
        context become processNewBlockMessages(unprocessedNewBlocks.tail, toBroadcastBlocks)
    }
  }
}

object BlockBroadcastActor {
  def props(peerActor: ActorRef, peerManagerActor: ActorRef, storage: Storage): Props = {
    Props(new BlockBroadcastActor(peerActor, peerManagerActor, storage))
  }

  case object StartBlockBroadcast
  case object ProcessNewBlocks

  /**TODO: Storages to add:
    *   Blockchain (for inserting blocks to it)
    */
  case class Storage(blockHeadersStorage: BlockHeadersStorage,
                     blockBodiesStorage: BlockBodiesStorage,
                     totalDifficultyStorage: TotalDifficultyStorage)
}

//FIXME: Start using [EC-43] Block class
case class Block(blockHeader: BlockHeader, blockBody: BlockBody)
