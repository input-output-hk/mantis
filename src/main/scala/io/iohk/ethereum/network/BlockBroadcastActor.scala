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
      context become processNewBlockMessages(ProcessingState(Seq(), Seq(), Seq(), Seq()))
  }

  def processNewBlockMessages(state: ProcessingState): Receive =
    handleMessages(state) orElse {

      case ProcessNewBlocks if state.unprocessedNewBlocks.nonEmpty =>
        val blockToProcess = state.unprocessedNewBlocks.head
        log.debug("Processing new block {}", blockToProcess)

        val block: Option[Block] = for {
          _ <- blockHeadersStorage.get(blockToProcess.blockHeader.parentHash)
          _ <- blockBodiesStorage.get(blockToProcess.blockHeader.parentHash)
        //TODO: Validate block header [EC-78] (using block parent)
        //TODO: Import block to blockchain
        } yield blockToProcess
        if(state.unprocessedNewBlocks.tail.nonEmpty) self ! ProcessNewBlocks
        block match {
          case Some(_) =>
            peerManagerActor ! GetPeers
            val newState = state.copy(
              unprocessedNewBlocks = state.unprocessedNewBlocks.tail,
              toBroadcastBlocks = state.toBroadcastBlocks :+ blockToProcess
            )
            context become processNewBlockMessages(newState)
          case None =>
            context become processNewBlockMessages(state.copy(unprocessedNewBlocks = state.unprocessedNewBlocks.tail))
        }

      case PeerManagerActor.PeersResponse(peers) if state.toBroadcastBlocks.nonEmpty =>
        state.toBroadcastBlocks.foreach{ b =>
          val parentTd = totalDifficultyStorage.get(b.blockHeader.parentHash)
            .getOrElse(throw new Exception("Block td is not on storage"))
          val newBlockMsg = NewBlock(b.blockHeader, b.blockBody, b.blockHeader.difficulty + parentTd)
          sendNewBlockMsgToPeers(peers, newBlockMsg)
        }
        context become processNewBlockMessages(state.copy(toBroadcastBlocks = Seq()))

      case _ => //Nothing
    }

  //TODO: Be able to process NewBlockHashes
  private def handleMessages(state: ProcessingState): Receive = {
    case PeerActor.MessageReceived(m: NewBlock) =>
      val newBlock = Block(m.blockHeader, m.blockBody)
      if(toProcess(newBlock.blockHeader.hash, state)){
        log.debug("Got NewBlock message {}", m)
        self ! ProcessNewBlocks
        context become processNewBlockMessages(state.copy(unprocessedNewBlocks = state.unprocessedNewBlocks :+ newBlock))
      }

    //FIXME: This is how it is implemented in geth but not in EthereumJ
    case PeerActor.MessageReceived(m: NewBlockHashes) =>
      val hashes = m.hashes.filter(toProcess(_, state))
      hashes.foreach{ hash => sender() ! PeerActor.SendMessage(GetBlockHeaders(Right(hash), 1, 0, false)) }
      context become processNewBlockMessages(state.copy(fetchedBlockHeaders = state.fetchedBlockHeaders ++ hashes))

    case PeerActor.MessageReceived(BlockHeaders(Seq(bh))) if state.fetchedBlockHeaders.contains(bh.hash)=>
      val newFetchedBlockHeaders = state.fetchedBlockHeaders.filterNot(_ == bh.hash)
      peer ! PeerActor.SendMessage(GetBlockBodies(Seq(bh.hash)))
      context become processNewBlockMessages(state.copy(fetchedBlockHeaders = newFetchedBlockHeaders, obtainedBlockHeaders = state.obtainedBlockHeaders :+ bh))

    case PeerActor.MessageReceived(BlockBodies(Seq(bb))) =>
      val block: Option[Block] = BlockHeaderAndBodyGrouper.matchHeaderAndBody(state.obtainedBlockHeaders, bb)
      block foreach { b =>
        val newObtainedBlockHeaders = state.obtainedBlockHeaders.filterNot(_.hash == b.blockHeader.hash)
        val newState = state.copy(unprocessedNewBlocks = state.unprocessedNewBlocks :+ b, obtainedBlockHeaders = newObtainedBlockHeaders)
        context become processNewBlockMessages(newState)
      }

  }

  def blockInProgress(hash: BlockHash, state: ProcessingState): Boolean =
    ((state.unprocessedNewBlocks ++ state.toBroadcastBlocks).map(_.blockHeader.hash) ++
      state.fetchedBlockHeaders ++
      state.obtainedBlockHeaders.map(_.hash)).contains(hash)

  def blockInStorage(hash: BlockHash, state: ProcessingState): Boolean = blockHeadersStorage.get(hash).isDefined

  //TODO: Check if the block's number is ok (not too old and not too into the future)
  //      We need to know our current height (from blockchain)
  //      We also need to decide how to handle blocks too into the future (Geth or EthereumJ approach)
  def toProcess(hash: BlockHash, state: ProcessingState): Boolean = !blockInProgress(hash, state) && !blockInStorage(hash, state)

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

  case class ProcessingState(unprocessedNewBlocks: Seq[Block],
                             toBroadcastBlocks: Seq[Block],
                             fetchedBlockHeaders: Seq[BlockHash],
                             obtainedBlockHeaders: Seq[BlockHeader])
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
