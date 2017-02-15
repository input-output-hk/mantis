package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import io.iohk.ethereum.db.storage.ReceiptStorage.BlockHash
import io.iohk.ethereum.db.storage.{BlockBodiesStorage, BlockHeadersStorage, TotalDifficultyStorage}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV61.NewBlockHashes
import io.iohk.ethereum.network.p2p.messages.PV62._

//TODO: Handle known blocks to peers [EC-80]
//FIXME: Should we be able unsubscribe from the messages that are not needed?
class BlockBroadcastActor(
                           peer: ActorRef,
                           peerManagerActor: ActorRef,
                           blockHeadersStorage: BlockHeadersStorage,
                           blockBodiesStorage: BlockBodiesStorage,
                           totalDifficultyStorage: TotalDifficultyStorage
                         ) extends Actor with ActorLogging {

  import BlockBroadcastActor._

  override def receive: Receive = idle

  def idle: Receive = {
    case StartBlockBroadcast =>
      peer ! PeerActor.Subscribe(Set(NewBlock.code, NewBlockHashes.code, BlockHeaders.code, BlockBodies.code))
      context become processNewBlockMessages(Seq(), Seq(), Seq(), Seq())
  }

  def processNewBlockMessages(unprocessedNewBlocks: Seq[Block],
                              toBroadcastBlocks: Seq[Block],
                              fetchedBlockHeaders: Seq[BlockHash],
                              obtainedBlockHeaders: Seq[BlockHeader]): Receive =
    handleMessages(unprocessedNewBlocks, toBroadcastBlocks, fetchedBlockHeaders, obtainedBlockHeaders) orElse {

      case ProcessNewBlocks if unprocessedNewBlocks.nonEmpty =>
        val blockToProcess = unprocessedNewBlocks.head
        log.debug("Processing new block {}", blockToProcess)

        val block: Option[Block] = for {
          _ <- blockHeadersStorage.get(blockToProcess.blockHeader.parentHash)
          _ <- blockBodiesStorage.get(blockToProcess.blockHeader.parentHash)
        //TODO: Validate block header [EC-78] (using block parent)
        //TODO: Import block to blockchain
        } yield blockToProcess

        if(unprocessedNewBlocks.tail.nonEmpty) self ! ProcessNewBlocks
        block match {
          case Some(_) =>
            peerManagerActor ! GetPeers
            context become processNewBlockMessages(
              unprocessedNewBlocks.tail,
              toBroadcastBlocks :+ blockToProcess,
              fetchedBlockHeaders,
              obtainedBlockHeaders
            )
          case None =>
            context become processNewBlockMessages(
              unprocessedNewBlocks.tail,
              toBroadcastBlocks,
              fetchedBlockHeaders,
              obtainedBlockHeaders
            )
        }

      case PeerManagerActor.PeersResponse(peers) if toBroadcastBlocks.nonEmpty =>
        toBroadcastBlocks.foreach{ b =>
          val parentTd = totalDifficultyStorage.get(b.blockHeader.parentHash)
            .getOrElse(throw new Exception("Block td is not on storage"))
          val newBlockMsg = NewBlock(b.blockHeader, b.blockBody, b.blockHeader.difficulty + parentTd)
          sendNewBlockMsgToPeers(peers, newBlockMsg)
        }
        context become processNewBlockMessages(unprocessedNewBlocks, Seq(), fetchedBlockHeaders, obtainedBlockHeaders)

      case _ => //Nothing
    }

  //TODO: Be able to process NewBlockHashes
  private def handleMessages(unprocessedNewBlocks: Seq[Block],
                             toBroadcastBlocks: Seq[Block],
                             fetchedBlockHeaders: Seq[BlockHash],
                             obtainedBlockHeaders: Seq[BlockHeader]): Receive = {

    case PeerActor.MessageReceived(m: NewBlock) =>
      //Check that the NewBlock should be processed
      val newBlock = Block(m.blockHeader, m.blockBody)
      val blockNotProcessed = !(unprocessedNewBlocks ++ toBroadcastBlocks).contains(newBlock)
      val blockNotInStorage = blockHeadersStorage.get(m.blockHeader.hash).isEmpty

      //TODO: Check if the block's number is ok (not too old and not too into the future)
      //      We need to know our current height (from blockchain)
      //      We also need to decide how to handle blocks too into the future (Geth or EthereumJ approach)

      if(blockNotProcessed && blockNotInStorage){
        log.debug("Got NewBlock message {}", m)
        self ! ProcessNewBlocks
        context become processNewBlockMessages(unprocessedNewBlocks :+ newBlock, toBroadcastBlocks, fetchedBlockHeaders, obtainedBlockHeaders)
      }

    //FIXME: This is how it is implemented in geth but not in EthereumJ
    case PeerActor.MessageReceived(m: NewBlockHashes) =>
      m.hashes.foreach{ hash => sender() ! PeerActor.SendMessage(GetBlockHeaders(Right(hash), 1, 0, false)) }
      context become processNewBlockMessages(unprocessedNewBlocks, toBroadcastBlocks, fetchedBlockHeaders ++ m.hashes, obtainedBlockHeaders)

    case PeerActor.MessageReceived(BlockHeaders(Seq(bh))) if fetchedBlockHeaders.contains(bh.hash)=>
      val newFetchedBlockHeaders = fetchedBlockHeaders.filterNot(_ == bh.hash)
      peer ! PeerActor.SendMessage(GetBlockBodies(Seq(bh.hash)))
      context become processNewBlockMessages(unprocessedNewBlocks, toBroadcastBlocks, newFetchedBlockHeaders, obtainedBlockHeaders :+ bh)

    case PeerActor.MessageReceived(BlockBodies(Seq(bb))) =>
      val block: Option[Block] = BlockHeaderAndBodyGrouper.matchHeaderAndBody(obtainedBlockHeaders, bb)
      block foreach { b =>
        val newObtainedBlockHeaders = obtainedBlockHeaders.filterNot(_.hash == b.blockHeader.hash)
        context become processNewBlockMessages(unprocessedNewBlocks :+ b, toBroadcastBlocks, fetchedBlockHeaders, newObtainedBlockHeaders)
      }

  }

  //TODO: Decide block propagation algorithm (for now we send block to every peer except the sender)
  private def sendNewBlockMsgToPeers(peers: Seq[Peer], newBlockMsg: NewBlock) = {
    peers.foreach{ p =>
      if(p.id != peer.path.name){
        log.debug("Sending NewBlockMessage {} to {}", newBlockMsg, p.id)
        p.ref ! PeerActor.SendMessage(newBlockMsg)
      }
    }
  }
}

object BlockBroadcastActor {
  def props(peer: ActorRef,
            peerManagerActor: ActorRef,
            blockHeadersStorage: BlockHeadersStorage,
            blockBodiesStorage: BlockBodiesStorage,
            totalDifficultyStorage: TotalDifficultyStorage): Props = {
    Props(new BlockBroadcastActor(peer, peerManagerActor, blockHeadersStorage, blockBodiesStorage, totalDifficultyStorage))
  }

  case object StartBlockBroadcast
  case object ProcessNewBlocks
}

//FIXME: Start using [EC-43] Block class and Block validator
case class Block(blockHeader: BlockHeader, blockBody: BlockBody){
  def isValid: Boolean = true
}

object BlockHeaderAndBodyGrouper {

  def matchHeaderAndBody(blockHeaders: Seq[BlockHeader], blockBody: BlockBody): Option[Block] =
    blockHeaders.collectFirst{ case blockHeader if Block(blockHeader, blockBody).isValid =>
      Block(blockHeader, blockBody)
    }

}
