package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.db.storage.{BlockBodiesStorage, BlockHeadersStorage, TotalDifficultyStorage}
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.PeerActor.MessageReceived
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peer}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.{PV61, PV62}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.network.p2p.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.network.p2p.validators.{BlockHeaderValidator, BlockValidator}
import io.iohk.ethereum.utils.{BlockchainStatus, NodeStatus}
import org.spongycastle.util.encoders.Hex

//FIXME: Handle known blocks to peers [EC-80]
class BlockBroadcastActor(
  nodeStatusHolder: Agent[NodeStatus],
  peer: ActorRef,
  peerManagerActor: ActorRef,
  blockHeadersStorage: BlockHeadersStorage,
  blockBodiesStorage: BlockBodiesStorage,
  totalDifficultyStorage: TotalDifficultyStorage) extends Actor with ActorLogging {

  import BlockBroadcastActor._

  override def receive: Receive = idle

  private val msgsToSubscribe = Set(NewBlock.code, PV61.NewBlockHashes.code, PV62.NewBlockHashes.code, BlockHeaders.code, BlockBodies.code)

  def idle: Receive = {
    case StartBlockBroadcast =>
      peer ! PeerActor.Subscribe(msgsToSubscribe)
      context become processMessages(ProcessingState(Seq(), Seq(), Seq(), Seq()))
  }

  def processMessages(state: ProcessingState): Receive =
    handleReceivedMessages(state) orElse {

      case ProcessNewBlocks if state.unprocessedBlocks.nonEmpty =>
        if(state.unprocessedBlocks.tail.nonEmpty) self ! ProcessNewBlocks

        val blockToProcess = state.unprocessedBlocks.head
        val blockHeader = BlockHeaderValidator.validate(blockToProcess.header, blockHeadersStorage)

        blockHeader match {
          case Right(_) =>
            importBlockToBlockchain(blockToProcess)

            peerManagerActor ! GetPeers
            val newState = state.copy(
              unprocessedBlocks = state.unprocessedBlocks.tail,
              toBroadcastBlocks = state.toBroadcastBlocks :+ blockToProcess
            )
            context become processMessages(newState)

          case Left(HeaderParentNotFoundError) =>
            log.info("Block parent not found, block {} will not be broadcasted", Hex.toHexString(blockToProcess.header.hash.toArray))
            context become processMessages(state.copy(unprocessedBlocks = state.unprocessedBlocks.tail))

          case Left(_) =>
            log.info("Block {} not valid, dropping peer {}", Hex.toHexString(blockToProcess.header.hash.toArray), peer.path.name)
            peer ! PeerActor.DropPeer(Disconnect.Reasons.UselessPeer)
            context stop self
        }

      case PeerManagerActor.PeersResponse(peers) if state.toBroadcastBlocks.nonEmpty =>
        state.toBroadcastBlocks.foreach{ b =>
          val newBlock = Block(b.header, b.body)
          sendNewBlockMsgToPeers(peers, newBlock)
        }
        context become processMessages(state.copy(toBroadcastBlocks = Seq()))

      case _ => //Nothing
    }

  def handleReceivedMessages(state: ProcessingState): Receive = {
    case MessageReceived(m: NewBlock) =>
      val newBlock = Block(m.block.header, m.block.body)
      if(blockToProcess(newBlock.header.hash, state)){
        log.info("Got NewBlock message {}", Hex.toHexString(newBlock.header.hash.toArray))
        self ! ProcessNewBlocks
        context become processMessages(state.copy(unprocessedBlocks = state.unprocessedBlocks :+ newBlock))
      }

    case MessageReceived(m: PV61.NewBlockHashes) => processNewBlockHashes(m.hashes, state)

    case MessageReceived(m: PV62.NewBlockHashes) => processNewBlockHashes(m.hashes.map(_.hash), state)

    case MessageReceived(BlockHeaders(Seq(blockHeader))) if state.fetchedBlockHeaders.contains(blockHeader.hash)=>
      log.info("Got BlockHeaders message {}", blockHeader)
      val newFetchedBlockHeaders = state.fetchedBlockHeaders.filterNot(_ == blockHeader.hash)
      peer ! PeerActor.SendMessage(GetBlockBodies(Seq(blockHeader.hash)))
      val newState = state.copy(fetchedBlockHeaders = newFetchedBlockHeaders, blockHeaders = state.blockHeaders :+ blockHeader)
      context become processMessages(newState)

    case MessageReceived(BlockBodies(Seq(blockBody))) =>
      val block: Option[Block] = matchHeaderAndBody(state.blockHeaders, blockBody)
      block foreach { b =>
        log.info("Got BlockBodies message {}", blockBody)
        val newBlockHeaders = state.blockHeaders.filterNot(_.hash == b.header.hash)
        val newState = state.copy(unprocessedBlocks = state.unprocessedBlocks :+ b, blockHeaders = newBlockHeaders)
        self ! ProcessNewBlocks
        context become processMessages(newState)
      }

  }

  private def processNewBlockHashes(newHashes: Seq[BlockHash], state: ProcessingState) = {
    val hashes = newHashes.filter(hash => blockToProcess(hash, state))
    log.info("Got NewBlockHashes message {}", newHashes.map( hash => Hex.toHexString(hash.toArray)))
    hashes.foreach{ hash =>
      val getBlockHeadersMsg = GetBlockHeaders(block = Right(hash), maxHeaders = 1, skip = 0, reverse =  false)
      peer ! PeerActor.SendMessage(getBlockHeadersMsg) }
    context become processMessages(state.copy(fetchedBlockHeaders = state.fetchedBlockHeaders ++ hashes))
  }

  private def blockInProgress(hash: BlockHash, state: ProcessingState): Boolean =
    ((state.unprocessedBlocks ++ state.toBroadcastBlocks).map(_.header.hash) ++
      state.fetchedBlockHeaders ++
      state.blockHeaders.map(_.hash)).contains(hash)

  private def blockInStorage(hash: BlockHash, state: ProcessingState): Boolean =
    blockHeadersStorage.get(hash).isDefined && blockBodiesStorage.get(hash).isDefined

  private def blockToProcess(hash: BlockHash, state: ProcessingState): Boolean = !blockInProgress(hash, state) && !blockInStorage(hash, state)

  //FIXME: Decide block propagation algorithm (for now we send block to every peer except the sender) [EC-87]
  private def sendNewBlockMsgToPeers(peers: Seq[Peer], newBlock: Block) = {
    val blockTd = totalDifficultyStorage.get(newBlock.header.hash)
      .getOrElse(throw new Exception(s"Block ${Hex.toHexString(newBlock.header.hash.toArray)} total difficulty is not on storage"))
    val newBlockMsg = NewBlock(newBlock, blockTd)
    peers.foreach{ p =>
      if(p.id != peer.path.name){
        log.info("Sending NewBlockMessage {} to {}", newBlockMsg, p.id)
        p.ref ! PeerActor.SendMessage(newBlockMsg)
      }
    }
  }

  private def matchHeaderAndBody(blockHeaders: Seq[BlockHeader], blockBody: BlockBody): Option[Block] =
    blockHeaders.collectFirst{ case blockHeader if BlockValidator.validateHeaderAndBody(blockHeader, blockBody).isRight =>
      Block(blockHeader, blockBody)
    }

  //FIXME: Replace with importing the block to blockchain
  private def importBlockToBlockchain(block: Block) = {
    val blockHash = block.header.hash

    //Insert block to storages
    blockHeadersStorage.put(blockHash, block.header)
    blockBodiesStorage.put(blockHash, block.body)
    val parentTd = totalDifficultyStorage.get(block.header.parentHash)
      .getOrElse(throw new Exception(s"Block ${Hex.toHexString(block.header.parentHash.toArray)} total difficulty is not on storage"))
    totalDifficultyStorage.put(blockHash, parentTd + block.header.difficulty)

    //Update NodeStatus
    nodeStatusHolder.send(_.copy(
      blockchainStatus = BlockchainStatus(block.header.difficulty, block.header.hash, block.header.number)))
  }
}

object BlockBroadcastActor {
  type BlockHash = ByteString

  def props(nodeStatusHolder: Agent[NodeStatus],
            peer: ActorRef,
            peerManagerActor: ActorRef,
            blockHeadersStorage: BlockHeadersStorage,
            blockBodiesStorage: BlockBodiesStorage,
            totalDifficultyStorage: TotalDifficultyStorage): Props = {
    Props(new BlockBroadcastActor(nodeStatusHolder, peer, peerManagerActor, blockHeadersStorage, blockBodiesStorage, totalDifficultyStorage))
  }

  case object StartBlockBroadcast
  case object ProcessNewBlocks

  case class ProcessingState(unprocessedBlocks: Seq[Block],
                             toBroadcastBlocks: Seq[Block],
                             fetchedBlockHeaders: Seq[BlockHash],
                             blockHeaders: Seq[BlockHeader])
}

