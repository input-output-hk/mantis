package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import io.iohk.ethereum.db.storage.{BlockBodiesStorage, BlockHeadersStorage}
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

  def receive: Receive = {
    //TODO: Be able to process NewBlockHashes
    case StartBlockBroadcast =>
      peerActor ! PeerActor.Subscribe(Set(NewBlock.code))
      context become processNewBlockMessages(Seq(), Seq())
  }

  def processNewBlockMessages(unprocessedNewBlocks: Seq[(Block, BigInt)], toBroadcastBlocks: Seq[(Block, BigInt)]): Receive = {

    case PeerActor.MessageReceived(m: NewBlock) =>
      //Check that the NewBlock should be processed
      val newBlock = Block(m.blockHeader, m.blockBody)
      val blockNotProcessed = (unprocessedNewBlocks ++ toBroadcastBlocks).forall(_._1 != newBlock)
      val blockNotInStorage = storage.blockHeadersStorage.get(m.blockHeader.hash).isEmpty

      //TODO: Check if the block's number is ok (not too old and not too into the future)
      //      We need to know our current height (from blockchain)
      //      We also need to decide how to handle blocks too into the future (Geth or EthereumJ approach)

      if(blockNotProcessed && blockNotInStorage){
        log.info("Got NewBlock message {}", m)
        self ! ProcessNewBlocks
        context become processNewBlockMessages(
          unprocessedNewBlocks = unprocessedNewBlocks :+ (newBlock -> m.totalDifficulty),
          toBroadcastBlocks = toBroadcastBlocks
        )
      }

    case ProcessNewBlocks if unprocessedNewBlocks.nonEmpty => processBlock(unprocessedNewBlocks, toBroadcastBlocks)

    case peers: Map[String, Peer] if toBroadcastBlocks.nonEmpty =>
      //TODO: Decide block propagation algorithm (for now we send block to every peer)
      peers.values.foreach{ peer =>
        toBroadcastBlocks.foreach{ case (b, td) =>
          val newBlockMsg = NewBlock(b.blockHeader, b.blockBody, td)
          log.info("Sending NewBlockMessage {} to {}", newBlockMsg, peer.id)
          peer.ref ! PeerActor.SendMessage(newBlockMsg)
        }
      }
      context become processNewBlockMessages(unprocessedNewBlocks, Seq())

    case other => //Nothing
      log.info("Received {}")
  }

  private def processBlock(unprocessedNewBlocks: Seq[(Block, BigInt)], toBroadcastBlocks: Seq[(Block, BigInt)]) = {
    log.info("Processing new block message")
    val (blockToProcess, blockTd) = unprocessedNewBlocks.head
    val blockToProcessHash = blockToProcess.blockHeader.hash

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
        context become processNewBlockMessages(unprocessedNewBlocks.tail, toBroadcastBlocks :+ (blockToProcess -> blockTd))
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
    *   TotalDifficultyStorage (for the blocks received after NewBlockHashes)
    *   Blockchain (for inserting blocks to it)
    */
  case class Storage(blockHeadersStorage: BlockHeadersStorage,
                     blockBodiesStorage: BlockBodiesStorage)
}

//FIXME: Start using [EC-43] Block class
case class Block(blockHeader: BlockHeader, blockBody: BlockBody)
