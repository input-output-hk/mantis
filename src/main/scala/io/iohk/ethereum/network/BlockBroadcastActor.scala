package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import io.iohk.ethereum.db.storage.{BlockBodiesStorage, BlockHeadersStorage, TotalDifficultyStorage}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerActor.MessageReceived
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.{PV61, PV62}
import io.iohk.ethereum.network.p2p.messages.PV62._
import org.spongycastle.util.encoders.Hex

//TODO: Handle known blocks to peers [EC-80]
class BlockBroadcastActor(
                           peer: ActorRef,
                           peerManagerActor: ActorRef,
                           blockHeadersStorage: BlockHeadersStorage,
                           blockBodiesStorage: BlockBodiesStorage,
                           totalDifficultyStorage: TotalDifficultyStorage
                         ) extends Actor with ActorLogging {

  import BlockBroadcastActor._

  override def receive: Receive = idle

  private val msgToSubscribe = Set(NewBlock.code, PV61.NewBlockHashes.code, PV62.NewBlockHashes.code, BlockHeaders.code, BlockBodies.code)

  def idle: Receive = {
    case StartBlockBroadcast =>
      peer ! PeerActor.Subscribe(msgToSubscribe)
      context become processMessages(ProcessingState(Seq(), Seq(), Seq(), Seq()))
  }

  def processMessages(state: ProcessingState): Receive =
    handleReceivedMessages(state) orElse {

      case ProcessNewBlocks if state.unprocessedNewBlocks.nonEmpty =>
        val blockToProcess = state.unprocessedNewBlocks.head
        log.debug("Processing new block {}", blockToProcess)

        val block: Option[Block] = for {
          _ <- blockHeadersStorage.get(blockToProcess.blockHeader.parentHash)
          _ <- blockBodiesStorage.get(blockToProcess.blockHeader.parentHash)
          //TODO: Validate block header [EC-78] (using block parent)
        } yield blockToProcess
        if(state.unprocessedNewBlocks.tail.nonEmpty) self ! ProcessNewBlocks
        block match {
          case Some(_) =>
            //FIXME: Replace with importing the block to blockchain
            blockHeadersStorage.put(blockToProcess.blockHeader.hash, blockToProcess.blockHeader)
            blockBodiesStorage.put(blockToProcess.blockHeader.hash, blockToProcess.blockBody)
            val parentTd = totalDifficultyStorage.get(blockToProcess.blockHeader.parentHash)
              .getOrElse(throw new Exception("Block total difficulty is not on storage"))
            totalDifficultyStorage.put(blockToProcess.blockHeader.hash, parentTd + blockToProcess.blockHeader.difficulty)

            peerManagerActor ! GetPeers
            val newState = state.copy(
              unprocessedNewBlocks = state.unprocessedNewBlocks.tail,
              toBroadcastBlocks = state.toBroadcastBlocks :+ blockToProcess
            )
            context become processMessages(newState)
          case None =>
            context become processMessages(state.copy(unprocessedNewBlocks = state.unprocessedNewBlocks.tail))
        }

      case PeerManagerActor.PeersResponse(peers) if state.toBroadcastBlocks.nonEmpty =>
        state.toBroadcastBlocks.foreach{ b =>
          val newBlock = Block(b.blockHeader, b.blockBody)
          sendNewBlockMsgToPeers(peers, newBlock)
        }
        context become processMessages(state.copy(toBroadcastBlocks = Seq()))

      case _ => //Nothing
    }

  def handleReceivedMessages(state: ProcessingState): Receive = {
    case MessageReceived(m: NewBlock) =>
      val newBlock = Block(m.blockHeader, m.blockBody)
      if(blockToProcess(newBlock.blockHeader.hash, state)){
        log.debug("Got NewBlock message {}", Hex.toHexString(newBlock.blockHeader.hash.toArray))
        self ! ProcessNewBlocks
        context become processMessages(state.copy(unprocessedNewBlocks = state.unprocessedNewBlocks :+ newBlock))
      }

    case MessageReceived(m: PV61.NewBlockHashes) => processNewBlockHashes(m.hashes, state)

    case MessageReceived(m: PV62.NewBlockHashes) => processNewBlockHashes(m.hashes.map(_.hash), state)

    case MessageReceived(BlockHeaders(Seq(blockHeader))) if state.fetchedBlockHeaders.contains(blockHeader.hash)=>
      val newFetchedBlockHeaders = state.fetchedBlockHeaders.filterNot(_ == blockHeader.hash)
      peer ! PeerActor.SendMessage(GetBlockBodies(Seq(blockHeader.hash)))
      val newState = state.copy(fetchedBlockHeaders = newFetchedBlockHeaders, obtainedBlockHeaders = state.obtainedBlockHeaders :+ blockHeader)
      context become processMessages(newState)

    case MessageReceived(BlockBodies(Seq(blockBody))) =>
      val block: Option[Block] = matchHeaderAndBody(state.obtainedBlockHeaders, blockBody)
      block foreach { b =>
        val newObtainedBlockHeaders = state.obtainedBlockHeaders.filterNot(_.hash == b.blockHeader.hash)
        val newState = state.copy(unprocessedNewBlocks = state.unprocessedNewBlocks :+ b, obtainedBlockHeaders = newObtainedBlockHeaders)
        self ! ProcessNewBlocks
        context become processMessages(newState)
      }

  }

  private def processNewBlockHashes(newHashes: Seq[BlockHash], state: ProcessingState) = {
    log.debug("Got NewBlockHashes message {}", newHashes)
    val hashes = newHashes.filter(hash => blockToProcess(hash, state))
    hashes.foreach{ hash =>
      val getBlockHeadersMsg = GetBlockHeaders(block = Right(hash), maxHeaders = 1, skip = 0, reverse =  false)
      peer ! PeerActor.SendMessage(getBlockHeadersMsg) }
    context become processMessages(state.copy(fetchedBlockHeaders = state.fetchedBlockHeaders ++ hashes))
  }

  private def blockInProgress(hash: BlockHash, state: ProcessingState): Boolean =
    ((state.unprocessedNewBlocks ++ state.toBroadcastBlocks).map(_.blockHeader.hash) ++
      state.fetchedBlockHeaders ++
      state.obtainedBlockHeaders.map(_.hash)).contains(hash)

  private def blockInStorage(hash: BlockHash, state: ProcessingState): Boolean =
    blockHeadersStorage.get(hash).isDefined && blockBodiesStorage.get(hash).isDefined

  //TODO: Check if the block's number is ok (not too old and not too into the future)
  //      We need to know our current height (from blockchain)
  private def blockToProcess(hash: BlockHash, state: ProcessingState): Boolean = !blockInProgress(hash, state) && !blockInStorage(hash, state)

  //TODO: Decide block propagation algorithm (for now we send block to every peer except the sender)
  private def sendNewBlockMsgToPeers(peers: Seq[Peer], newBlock: Block) = {
    val parentTd = totalDifficultyStorage.get(newBlock.blockHeader.parentHash)
      .getOrElse(throw new Exception("Block td is not on storage"))
    val newBlockMsg = NewBlock(newBlock.blockHeader, newBlock.blockBody, newBlock.blockHeader.difficulty + parentTd)
    peers.foreach{ p =>
      if(p.id != peer.path.name){
        log.debug("Sending NewBlockMessage {} to {}", newBlockMsg, p.id)
        p.ref ! PeerActor.SendMessage(newBlockMsg)
      }
    }
  }

  private def matchHeaderAndBody(blockHeaders: Seq[BlockHeader], blockBody: BlockBody): Option[Block] =
    blockHeaders.collectFirst{ case blockHeader if Block(blockHeader, blockBody).isValid =>
      Block(blockHeader, blockBody)
    }
}

object BlockBroadcastActor {
  type BlockHash = ByteString

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

//TODO: Start using [EC-43] Block class and Block validator
case class Block(blockHeader: BlockHeader, blockBody: BlockBody){
  def isValid: Boolean = true
}
