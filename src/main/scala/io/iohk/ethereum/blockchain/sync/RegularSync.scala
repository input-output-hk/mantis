package io.iohk.ethereum.blockchain.sync

import java.io.FileWriter

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlacklistPeer
import io.iohk.ethereum.blockchain.sync.SyncRequestHandler.Done
import io.iohk.ethereum.blockchain.sync.SyncController.{BlockBodiesReceived, BlockHeadersReceived, BlockHeadersToResolve, PrintStatus}
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerActor._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.utils.Config
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.rlp._

import scala.collection.immutable
import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global

trait RegularSync {
  selfSyncController: SyncController =>

  private var headersQueue: Seq[BlockHeader] = Seq.empty
  private var broadcasting = false
  private var waitingForActor: Option[ActorRef] = None

  import Config.FastSync._

  def startRegularSync(): Unit = {
    log.info("Starting regular sync")
    appStateStorage.fastSyncDone()
    context become (handlePeerUpdates orElse regularSync())
    askForHeaders()
  }
  // scalastyle:off
  var stateNodesHashes: Set[ByteString] = Set.empty
  var contractNodesHashes: Set[ByteString] = Set.empty
  var evmCodeHashes: Set[ByteString] = Set.empty

  var blockHeadersStorage: Map[ByteString, BlockHeader] = HashMap.empty
  var blockBodyStorage: Map[ByteString, BlockBody] = HashMap.empty
  var blockReceiptsStorage: Map[ByteString, Seq[Receipt]] = HashMap.empty
  var stateStorage: Map[ByteString, MptNode] = HashMap.empty
  var contractStorage: Map[ByteString, MptNode] = HashMap.empty
  var evmCodeStorage: Map[ByteString, ByteString] = HashMap.empty

  def regularSync(): Receive = {
    case ResumeRegularSync =>
      askForHeaders()

    case BlockHeadersToResolve(peer, headers) =>
      waitingForActor = None
      handleBlockBranchResolution(peer, headers)

    case BlockHeadersReceived(peer, headers) =>
      waitingForActor = None
      handleDownload(peer, headers)

    case BlockBodiesReceived(peer, _, blockBodies) =>
      waitingForActor = None
      handleBlockBodies(peer, blockBodies)

    case block: BroadcastBlocks if broadcasting =>
      //FIXME: Decide block propagation algorithm (for now we send block to every peer) [EC-87]
      peersToDownloadFrom.keys.foreach(_ ! block)

    case PrintStatus =>
      log.info(s"Peers: ${handshakedPeers.size} (${blacklistedPeers.size} blacklisted).")

    case Done =>
      if (waitingForActor == Option(sender())) {
        //actor is done and we did not get response
        scheduleResume()
      }

    case MessageReceived(m:BlockHeaders) =>
      val encodedHeaders = m.headers.map(BlockHeaderImplicits.headerRlpEncDec.encode).map(encode).map(Hex.toHexString)
      val headerHashes = m.headers.map(_.hash)
      val mptRoots: Seq[ByteString] = m.headers.map(_.stateRoot)

      m.headers.foreach { h =>
        blockHeadersStorage = blockHeadersStorage + (h.hash -> h)
      }

      handshakedPeers.headOption.foreach { case (actor, _) =>
        actor ! SendMessage(GetBlockBodies(headerHashes))
        actor ! SendMessage(GetReceipts(headerHashes.drop(1)))
        actor ! SendMessage(GetNodeData(mptRoots))
        stateNodesHashes = stateNodesHashes ++ mptRoots.toSet
      }

    case MessageReceived(m: BlockBodies) =>
      m.bodies.zip(blockHeadersStorage.keys).foreach { case (b, h) =>
        blockBodyStorage = blockBodyStorage + (h -> b)
      }


    case MessageReceived(m: Receipts) =>
      m.receiptsForBlocks.zip(blockHeadersStorage.keys).foreach { case (r, h) =>
        blockReceiptsStorage = blockReceiptsStorage + (h -> r)
      }


    case MessageReceived(m: NodeData) =>
      val emptyStorage = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))
      val emptyEvm = ByteString(Hex.decode("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

      val stateNodes = m.values.filter(node => stateNodesHashes.contains(kec256(node)))
      val contractNodes = m.values.filter(node => contractNodesHashes.contains(kec256(node)))
      val evmCode = m.values.filter(node => evmCodeHashes.contains(kec256(node)))

      val nodes = NodeData(stateNodes).values.indices.map(i => NodeData(stateNodes).getMptNode(i))

      val children = nodes.flatMap {
        case n: MptBranch => n.children.collect { case Left(h: MptHash) if h.hash.nonEmpty => h.hash }
        case MptExtension(_, Left(h)) => Seq(h.hash)
        case n: MptLeaf => Seq.empty
        case _ => Seq.empty
      }

      var cChildren: Seq[ByteString] = Nil
      var evmTorequest: Seq[ByteString] = Nil

      nodes.foreach {
        case n: MptLeaf =>
          if(n.getAccount.codeHash != emptyEvm){
            handshakedPeers.headOption.foreach { case (actor, _) =>
              evmTorequest = evmTorequest :+ n.getAccount.codeHash
              evmCodeHashes = evmCodeHashes + n.getAccount.codeHash
            }
          }
          if(n.getAccount.storageRoot != emptyStorage){
            handshakedPeers.headOption.foreach { case (actor, _) =>
              cChildren = cChildren :+ n.getAccount.storageRoot
              contractNodesHashes = contractNodesHashes + n.getAccount.storageRoot
            }
          }
        case _ =>
      }

      val cNodes = NodeData(contractNodes).values.indices.map(i => NodeData(contractNodes).getMptNode(i))
      cChildren = cChildren ++ cNodes.flatMap {
        case n: MptBranch => n.children.collect { case Left(h: MptHash) if h.hash.nonEmpty => h.hash }
        case MptExtension(_, Left(h)) => Seq(h.hash)
        case _ => Seq.empty
      }
      handshakedPeers.headOption.foreach { case (actor, _) =>
        actor ! SendMessage(GetNodeData(children ++ cChildren ++ evmTorequest))
        stateNodesHashes = stateNodesHashes ++ children.toSet
        contractNodesHashes = contractNodesHashes ++ cChildren.toSet
      }

      evmCode.foreach{e=>
        evmCodeStorage = evmCodeStorage + (kec256(e) -> e)
      }

      nodes.foreach{n=>
        stateStorage = stateStorage + (n.hash -> n)
      }

      cNodes.foreach { n =>
        contractStorage = contractStorage + (n.hash -> n)
      }

      if(children.isEmpty && cChildren.isEmpty && evmTorequest.isEmpty){
        import BlockHeaderImplicits._
        import Receipt._
        import RLPImplicitConversions._

        val headersFile = new FileWriter("headers.txt", true)
        val bodiesFile = new FileWriter("bodies.txt", true)
        val receiptsFile = new FileWriter("receipts.txt", true)
        val stateTreeFile = new FileWriter("stateTree.txt", true)
        val contractTreesFile = new FileWriter("contractTrees.txt", true)
        val evmCodeFile = new FileWriter("evmCode.txt", true)

        def dumpToFile[T](fw: FileWriter, element: (ByteString, T))(implicit enc: RLPEncoder[T]): Unit = element match {
          case (h, v) => fw.write(s"${Hex.toHexString(h.toArray[Byte])} ${Hex.toHexString(encode(v))}\n")
        }

        blockHeadersStorage.foreach(dumpToFile(headersFile, _))
        blockBodyStorage.foreach(dumpToFile(bodiesFile, _))
        blockReceiptsStorage.foreach { case (h, v: Seq[Receipt]) =>  receiptsFile.write(s"${Hex.toHexString(h.toArray[Byte])} ${Hex.toHexString(encode(toRlpList(v)))}\n")}
        stateStorage.foreach(dumpToFile(stateTreeFile, _))
        contractStorage.foreach(dumpToFile(contractTreesFile, _))
        evmCodeStorage.foreach{case (h, v) => evmCodeFile.write(s"${Hex.toHexString(h.toArray[Byte])} ${Hex.toHexString(v.toArray[Byte])}\n")}

        headersFile.close()
        bodiesFile.close()
        receiptsFile.close()
        stateTreeFile.close()
        contractTreesFile.close()
        evmCodeFile.close()
        println("chain dumped to file")
      }
  }

  private def askForHeaders() = {
    if (handshakedPeers.isEmpty) {
      scheduleResume()
    } else {
      handshakedPeers.headOption.foreach { case (actor, _) =>
        actor ! Subscribe(Set(BlockHeaders.code, BlockBodies.code, Receipts.code, NodeData.code))
        actor ! SendMessage(GetBlockHeaders(Left(0), 10, 0, reverse = false))
      }
    }
//    bestPeer match {
//      case Some(peer) =>
//        val blockNumber = appStateStorage.getBestBlockNumber()
//        val request = GetBlockHeaders(Left(blockNumber + 1), blockHeadersPerRequest, skip = 0, reverse = false)
//        waitingForActor = Some(context.actorOf(SyncBlockHeadersRequestHandler.props(peer, request, resolveBranches = false)))
//      case None =>
//        log.warning("no peers to download from")
//        scheduleResume()
//    }
  }
  // scalastyle:on
  private def handleBlockBranchResolution(peer: ActorRef, message: Seq[BlockHeader]) =
    //todo limit max branch depth?
    if (message.nonEmpty && message.last.hash == headersQueue.head.parentHash) {
      headersQueue = message ++ headersQueue
      processBlockHeaders(peer, headersQueue)
    } else {
      //we did not get previous blocks, there is no way to resolve, blacklist peer and continue download
      resumeWithDifferentPeer(peer)
    }

  private def handleDownload(peer: ActorRef, message: Seq[BlockHeader]) = if (message.nonEmpty) {
    headersQueue = message
    processBlockHeaders(peer, message)
  } else {
    //no new headers to process, schedule to ask again in future, we are at the top of chain
    broadcasting = true
    scheduleResume()
  }

  private def processBlockHeaders(peer: ActorRef, headers: Seq[BlockHeader]) = {
    val parentByNumber = blockchain.getBlockHeaderByNumber(headers.head.number - 1)

    parentByNumber match {
      case Some(parent) if checkHeaders(headers) =>
        //we have same chain
        if (parent.hash == headers.head.parentHash) {
          val hashes = headersQueue.take(blockBodiesPerRequest).map(_.hash)
          waitingForActor = Some(context.actorOf(SyncBlockBodiesRequestHandler.props(peer, hashes)))
        } else {
          val request = GetBlockHeaders(Right(headersQueue.head.parentHash), blockResolveDepth, skip = 0, reverse = true)
          waitingForActor = Some(context.actorOf(SyncBlockHeadersRequestHandler.props(peer, request, resolveBranches = true)))
        }
      case _ =>
        log.warning("got header that does not have parent")
        resumeWithDifferentPeer(peer)
    }
  }

  private def handleBlockBodies(peer: ActorRef, m: Seq[BlockBody]) = {
    if (m.nonEmpty) {
      val result = headersQueue.zip(m).map { case (h, b) => blockValidator(h, b) }

      if (!result.exists(_.isLeft) && result.nonEmpty) {
        val blocks = result.collect { case Right(b) => b }

        blockchain.getBlockHeaderByHash(blocks.head.header.parentHash)
          .flatMap(b => blockchain.getTotalDifficultyByHash(b.hash)) match {
          case Some(td) =>
            var currentTd = td
            val newBlocks = blocks.map { b =>
              val blockHashToDelete = blockchain.getBlockHeaderByNumber(b.header.number).map(_.hash).filter(_ != b.header.hash)
              blockchain.save(b)
              appStateStorage.putBestBlockNumber(b.header.number)
              currentTd += b.header.difficulty
              blockchain.save(b.header.hash, currentTd)
              blockHashToDelete.foreach(blockchain.removeBlock)

              NewBlock(b, currentTd)
            }

            context.self ! BroadcastBlocks(newBlocks)
            log.info(s"got new blocks up till block: ${newBlocks.last.block.header.number} " +
              s"with hash ${Hex.toHexString(newBlocks.last.block.header.hash.toArray[Byte])}")
          case None =>
            log.error("no total difficulty for latest block")
        }

        headersQueue = headersQueue.drop(result.length)
        if (headersQueue.nonEmpty) {
          val hashes = headersQueue.take(blockBodiesPerRequest).map(_.hash)
          waitingForActor = Some(context.actorOf(SyncBlockBodiesRequestHandler.props(peer, hashes)))
        } else {
          context.self ! ResumeRegularSync
        }
      } else {
        //blacklist for errors in blocks
        resumeWithDifferentPeer(peer)
      }
    } else {
      //we got empty response for bodies from peer but we got block headers earlier
      resumeWithDifferentPeer(peer)
    }
  }

  private def scheduleResume() = {
    headersQueue = Seq.empty
    scheduler.scheduleOnce(checkForNewBlockInterval, context.self, ResumeRegularSync)
  }

  private def resumeWithDifferentPeer(currentPeer: ActorRef) = {
    self ! BlacklistPeer(currentPeer, "because of error in response")
    headersQueue = Seq.empty
    context.self ! ResumeRegularSync
  }

  private def checkHeaders(headers: Seq[BlockHeader]): Boolean =
    headers.zip(headers.tail).forall { case (parent, child) => parent.hash == child.parentHash && parent.number + 1 == child.number }

  private def bestPeer: Option[ActorRef] = {
    val peersToUse = peersToDownloadFrom
      .collect {
        case (ref, Handshaked(_, true, totalDifficulty)) => (ref, totalDifficulty)
      }

    if (peersToUse.nonEmpty) Some(peersToUse.maxBy { case (_, td) => td }._1)
    else None
  }

  private case object ResumeRegularSync
  private case class ResolveBranch(peer: ActorRef)
}
