package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain}
import io.iohk.ethereum.network.PeerActor.MessageReceived
import io.iohk.ethereum.network.GetPeersHandler.RequestPeers
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.network.p2p.validators.{BlockHeaderValidator, BlockValidator}
import io.iohk.ethereum.utils.NodeStatus
import org.spongycastle.util.encoders.Hex

//FIXME: Handle peers not responding to block headers and bodies request [EC-107]
class BlockBroadcastActor(
  nodeStatusHolder: Agent[NodeStatus],
  val peer: ActorRef,
  peerManagerActor: ActorRef,
  appStateStorage: AppStateStorage,
  blockchain: Blockchain) extends Actor with ActorLogging with GetPeersHandler with MaxBlockNumberRequestHandler {

  import BlockBroadcastActor._

  override def receive: Receive = idle

  private val msgsToSubscribe = Set(NewBlock.code, PV62.NewBlockHashes.code, BlockHeaders.code, BlockBodies.code)

  def idle: Receive = {
    case StartBlockBroadcast =>
      peer ! PeerActor.Subscribe(msgsToSubscribe)
      context become processMessages()
  }

  def processMessages(state: ProcessingState = ProcessingState.empty): Receive =
    handleReceivedMessages(state) orElse
      handleRequestMaxBlockNumber(state) orElse handlePeerMaxBlockNumber(state) orElse
      handleRequestPeers(state, peerManagerActor) orElse handlePeersResponse(state) orElse {

      case ProcessNewBlocks if state.unprocessedBlocks.nonEmpty =>
        if(state.unprocessedBlocks.tail.nonEmpty) self ! ProcessNewBlocks

        val blockToProcess = state.unprocessedBlocks.head
        val blockHeader = BlockHeaderValidator.validate(blockToProcess.header, blockchain)
        val blockParentTotalDifficulty = blockchain.getTotalDifficultyByHash(blockToProcess.header.parentHash)

        (blockHeader, blockParentTotalDifficulty) match {
          case (Right(_), Some(parentTD)) =>
            val blockToProcessDifficulty = parentTD + blockToProcess.header.difficulty
            importBlockToBlockchain(blockToProcess, blockToProcessDifficulty)

            val newBlockMsg = NewBlock(blockToProcess, blockToProcessDifficulty)
            val newState = state.withUnprocessBlocks(state.unprocessedBlocks.tail).withToBroadcastBlocks(state.toBroadcastBlocks :+ newBlockMsg)
            self ! RequestPeers
            context become processMessages(newState)

          case (Left(HeaderParentNotFoundError), _) | (Right(_), None) =>
            log.warning("Block parent not found, block {} will not be broadcasted", Hex.toHexString(blockToProcess.header.hash.toArray))
            context become processMessages(state.copy(unprocessedBlocks = state.unprocessedBlocks.tail))

          case (Left(_), _) =>
            log.warning("Block {} not valid", Hex.toHexString(blockToProcess.header.hash.toArray))
            context become processMessages(state.copy(unprocessedBlocks = state.unprocessedBlocks.tail))
        }

      case _ => //Nothing
    }

  def handleReceivedMessages(state: ProcessingState): Receive = {

    case MessageReceived(m: NewBlock) =>
      val newBlock = Block(m.block.header, m.block.body)
      if(blockToProcess(newBlock.header.hash, state)){
        log.debug("Got NewBlock message {}", Hex.toHexString(newBlock.header.hash.toArray))
        self ! ProcessNewBlocks
        context become processMessages(state.copy(unprocessedBlocks = state.unprocessedBlocks :+ newBlock))
      }

    case MessageReceived(m: PV62.NewBlockHashes) =>
      val newHashes = m.hashes.map(_.hash)
      val newHashesToProcess = newHashes.filter(hash => blockToProcess(hash, state))
      log.debug("Got NewBlockHashes message {}", newHashes.map( hash => Hex.toHexString(hash.toArray)))
      newHashesToProcess.foreach{ hash =>
        val getBlockHeadersMsg = GetBlockHeaders(block = Right(hash), maxHeaders = 1, skip = 0, reverse =  false)
        peer ! PeerActor.SendMessage(getBlockHeadersMsg)
      }
      context become processMessages(state.copy(fetchedBlockHashes = state.fetchedBlockHashes ++ newHashesToProcess))

    case MessageReceived(BlockHeaders(Seq(blockHeader))) if state.fetchedBlockHashes.contains(blockHeader.hash)=>
      log.debug("Got BlockHeaders message {}", blockHeader)
      val newFetchedBlockHeaders = state.fetchedBlockHashes.filterNot(_ == blockHeader.hash)
      peer ! PeerActor.SendMessage(GetBlockBodies(Seq(blockHeader.hash)))
      val newState = state.copy(fetchedBlockHashes = newFetchedBlockHeaders, blockHeaders = state.blockHeaders :+ blockHeader)
      context become processMessages(newState)

    case MessageReceived(BlockBodies(Seq(blockBody))) =>
      val block: Option[Block] = matchHeaderAndBody(state.blockHeaders, blockBody)
      block foreach { b =>
        log.debug("Got BlockBodies message {}", blockBody)
        val newBlockHeaders = state.blockHeaders.filterNot(_.hash == b.header.hash)
        val newState = state.copy(unprocessedBlocks = state.unprocessedBlocks :+ b, blockHeaders = newBlockHeaders)
        self ! ProcessNewBlocks
        context become processMessages(newState)
      }

  }

  private def blockInProgress(hash: BlockHash, state: ProcessingState): Boolean =
    (state.unprocessedBlocks.map(_.header.hash) ++
      state.toBroadcastBlocks.map(_.block.header.hash) ++
      state.fetchedBlockHashes ++
      state.blockHeaders.map(_.hash)).contains(hash)

  private def blockInStorage(hash: BlockHash): Boolean =
    blockchain.getBlockByHash(hash).isDefined

  private def blockToProcess(hash: BlockHash, state: ProcessingState): Boolean = !blockInProgress(hash, state) && !blockInStorage(hash)

  private def matchHeaderAndBody(blockHeaders: Seq[BlockHeader], blockBody: BlockBody): Option[Block] =
    blockHeaders.collectFirst{ case blockHeader if BlockValidator.validateHeaderAndBody(blockHeader, blockBody).isRight =>
      Block(blockHeader, blockBody)
    }

  private def importBlockToBlockchain(block: Block, blockTotalDifficulty: BigInt) = {
    val blockHash = block.header.hash

    //Insert block to blockchain
    blockchain.save(block)
    blockchain.save(blockHash, blockTotalDifficulty)

    //Update best block number in app state
    appStateStorage.putBestBlockNumber(block.header.number)
  }
}

object BlockBroadcastActor {
  type BlockHash = ByteString

  def props(nodeStatusHolder: Agent[NodeStatus],
            peer: ActorRef,
            peerManagerActor: ActorRef,
            appStateStorage: AppStateStorage,
            blockchain: Blockchain): Props = {
    Props(new BlockBroadcastActor(nodeStatusHolder, peer, peerManagerActor, appStateStorage, blockchain))
  }

  case object StartBlockBroadcast
  case object ProcessNewBlocks

  case class ProcessingState(unprocessedBlocks: Seq[Block],
                             toBroadcastBlocks: Seq[NewBlock],
                             toBroadcastBlocksToEachPeer: Map[ActorRef, Seq[NewBlock]],
                             fetchedBlockHashes: Seq[BlockHash],
                             blockHeaders: Seq[BlockHeader]) {

    def withToBroadcastBlocksToEachPeer(newToBroadcastBlocksToPeers: Map[ActorRef, Seq[NewBlock]]): ProcessingState =
      this.copy(toBroadcastBlocksToEachPeer = newToBroadcastBlocksToPeers)

    def withToBroadcastBlocks(newToBroadcastBlocks: Seq[NewBlock]): ProcessingState =
      this.copy(toBroadcastBlocks = newToBroadcastBlocks)

    def withUnprocessBlocks(newUnprocessedBlocks: Seq[Block]): ProcessingState =
      this.copy(unprocessedBlocks = newUnprocessedBlocks)
  }

  object ProcessingState {
    val empty: ProcessingState = ProcessingState(Nil, Nil, Map(), Nil, Nil)
  }
}
