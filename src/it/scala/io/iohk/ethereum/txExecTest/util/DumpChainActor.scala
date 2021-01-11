package io.iohk.ethereum.txExecTest.util

import java.io.FileWriter
import java.net.URI

import akka.actor.{Actor, ActorRef, _}
import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.domain.{BlockBody, BlockHeader, Receipt}
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, HashNode, LeafNode, MptNode}
import io.iohk.ethereum.network.PeerActor.SendMessage
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peers}
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._
import io.iohk.ethereum.network.p2p.messages.PV63.ReceiptImplicits._
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.network.{Peer, PeerManagerActor}
import io.iohk.ethereum.txExecTest.util.DumpChainActor._
import org.bouncycastle.util.encoders.Hex

import scala.collection.immutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * Actor used for obtaining all the blockchain data (blocks, receipts, nodes) from the blocks [startBlock, maxBlocks]
  * from a peer bootstrapNode.
  * The bootstrapNode is assumed to respond to all the messages and properly, so no validation of the received data is done.
  */
class DumpChainActor(
    peerManager: ActorRef,
    peerMessageBus: ActorRef,
    startBlock: BigInt,
    maxBlocks: BigInt,
    bootstrapNode: String
) extends Actor {
  var stateNodesHashes: Set[ByteString] = Set.empty
  var contractNodesHashes: Set[ByteString] = Set.empty
  var evmCodeHashes: Set[ByteString] = Set.empty

  //Temporary storages used to store the received data
  var blockHeadersStorage: Map[ByteString, BlockHeader] = HashMap.empty
  var blockBodyStorage: Map[ByteString, BlockBody] = HashMap.empty
  var blockReceiptsStorage: Map[ByteString, Seq[Receipt]] = HashMap.empty
  var stateStorage: Map[ByteString, MptNode] = HashMap.empty
  var contractStorage: Map[ByteString, MptNode] = HashMap.empty
  var evmCodeStorage: Map[ByteString, ByteString] = HashMap.empty

  //Pending data to request
  var blockHeaderToRequest: BigInt = 0
  var receiptsToRequest: Seq[ByteString] = Nil
  var blockBodiesToRequest: Seq[ByteString] = Nil
  var nodesToRequest: Seq[ByteString] = Nil

  var receiptsRequested: Seq[ByteString] = Nil
  var blockBodiesRequested: Seq[ByteString] = Nil

  var peers: Seq[Peer] = Nil

  override def preStart(): Unit = {
    val r: Runnable = () => peerManager ! GetPeers
    context.system.scheduler.scheduleOnce(4 seconds, r)
  }

  //Periodically try to connect to bootstrap peer in case the connection failed before dump termination
  val connectToBootstrapTimeout: Cancellable = context.system.scheduler.scheduleWithFixedDelay(
    0 seconds,
    4 seconds,
    peerManager,
    PeerManagerActor.ConnectToPeer(new URI(bootstrapNode))
  )

  val assignWorkTimeout: Cancellable =
    context.system.scheduler.scheduleWithFixedDelay(0 seconds, 2 seconds)(() => assignWork())

  // scalastyle:off
  override def receive: Receive = {
    case Peers(p) =>
      peers = p.keys.toSeq
      peers.headOption.foreach { peer =>
        peerMessageBus ! Subscribe(
          MessageClassifier(
            Set(Codes.BlockHeadersCode, Codes.BlockBodiesCode, Codes.ReceiptsCode, Codes.NodeDataCode),
            PeerSelector.WithId(peer.id)
          )
        )
      }

    case MessageFromPeer(m: BlockHeaders, _) =>
      println(s"Received ${m.headers.size} headers")
      val mptRoots: Seq[ByteString] = m.headers.map(_.stateRoot)

      m.headers.foreach { h =>
        blockHeadersStorage = blockHeadersStorage + (h.hash -> h)
      }

      blockBodiesToRequest = blockBodiesToRequest ++ m.headers.map(_.hash)
      receiptsToRequest = receiptsToRequest ++ m.headers.map(_.hash)
      nodesToRequest = nodesToRequest ++ mptRoots
      stateNodesHashes = stateNodesHashes ++ mptRoots.toSet

    case MessageFromPeer(m: BlockBodies, _) =>
      println(s"Received ${m.bodies.size} bodies")
      m.bodies.zip(blockBodiesRequested).foreach { case (b, h) =>
        blockBodyStorage = blockBodyStorage + (h -> b)
      }
      blockBodiesRequested = Nil

    case MessageFromPeer(m: Receipts, _) =>
      println(s"Received ${m.receiptsForBlocks.size} receipts lists")
      m.receiptsForBlocks.zip(receiptsRequested).foreach { case (r, h) =>
        blockReceiptsStorage = blockReceiptsStorage + (h -> r)
      }
      receiptsRequested = Nil

    case MessageFromPeer(m: NodeData, _) =>
      val stateNodes = m.values.filter(node => stateNodesHashes.contains(kec256(node)))
      val contractNodes = m.values.filter(node => contractNodesHashes.contains(kec256(node)))
      val evmCode = m.values.filter(node => evmCodeHashes.contains(kec256(node)))

      val nodes = NodeData(stateNodes).values.indices.map(i => NodeData(stateNodes).getMptNode(i))

      val children = nodes.flatMap {
        case n: BranchNode => n.children.collect { case HashNode(h) => ByteString(h) }
        case ExtensionNode(_, HashNode(h), _, _, _) => Seq(ByteString(h))
        case _: LeafNode => Seq.empty
        case _ => Seq.empty
      }

      var contractChildren: Seq[ByteString] = Nil
      var evmTorequest: Seq[ByteString] = Nil

      nodes.foreach {
        case n: LeafNode =>
          import AccountImplicits._
          val account = n.value.toArray[Byte].toAccount

          if (account.codeHash != DumpChainActor.emptyEvm) {
            peers.headOption.foreach { _ =>
              evmTorequest = evmTorequest :+ account.codeHash
              evmCodeHashes = evmCodeHashes + account.codeHash
            }
          }
          if (account.storageRoot != DumpChainActor.emptyStorage) {
            peers.headOption.foreach { _ =>
              contractChildren = contractChildren :+ account.storageRoot
              contractNodesHashes = contractNodesHashes + account.storageRoot
            }
          }
        case _ =>
      }

      val cNodes = NodeData(contractNodes).values.indices.map(i => NodeData(contractNodes).getMptNode(i))
      contractChildren = contractChildren ++ cNodes.flatMap {
        case n: BranchNode => n.children.collect { case HashNode(h) => ByteString(h) }
        case ExtensionNode(_, HashNode(h), _, _, _) => Seq(ByteString(h))
        case _ => Seq.empty
      }

      stateNodesHashes = stateNodesHashes ++ children.toSet
      contractNodesHashes = contractNodesHashes ++ contractChildren.toSet

      evmCode.foreach { e =>
        evmCodeStorage = evmCodeStorage + (kec256(e) -> e)
      }

      nodes.foreach { n =>
        stateStorage = stateStorage + (ByteString(n.hash) -> n)
      }

      cNodes.foreach { n =>
        contractStorage = contractStorage + (ByteString(n.hash) -> n)
      }

      nodesToRequest = nodesToRequest ++ children ++ contractChildren ++ evmTorequest

  }

  private def assignWork(): Unit = {
    if (!anyRequestsRemaining()) {
      dumpChainToFile()
      println("Finished download, dumped chain to file")
      assignWorkTimeout.cancel()
      connectToBootstrapTimeout.cancel()
      context stop self
    } else {
      if (peers.nonEmpty) {
        val peerToRequest = peers.head
        //Block headers are only requested once the pending receipts and bodies requests were finished
        if (
          blockHeaderToRequest < maxBlocks && receiptsRequested.isEmpty && blockBodiesRequested.isEmpty &&
          blockBodiesToRequest.isEmpty && receiptsToRequest.isEmpty
        ) {
          val headersRemaining = maxBlocks - blockHeaderToRequest
          peerToRequest.ref ! SendMessage(
            GetBlockHeaders(
              block = Left(blockHeaderToRequest),
              maxHeaders = headersRemaining.max(MaxHeadersPerRequest),
              skip = 0,
              reverse = false
            )
          )
          blockHeaderToRequest = blockHeaderToRequest + MaxHeadersPerRequest

        } else if (nodesToRequest.nonEmpty) {
          val (currentNodesToRequest, remainingNodesToRequest) = nodesToRequest.splitAt(MaxNodesPerRequest)
          nodesToRequest = remainingNodesToRequest
          peerToRequest.ref ! SendMessage(GetNodeData(currentNodesToRequest))

        } else if (blockBodiesToRequest.nonEmpty) {
          val (currentBlockBodiesToRequest, remainingBodiesToRequest) =
            blockBodiesToRequest.splitAt(MaxBodiesPerRequest)
          blockBodiesToRequest = remainingBodiesToRequest
          blockBodiesRequested = currentBlockBodiesToRequest
          peerToRequest.ref ! SendMessage(GetBlockBodies(currentBlockBodiesToRequest))

        } else if (receiptsToRequest.nonEmpty) {
          val (currentReceiptsToRequest, remainingReceiptsToRequest) = receiptsToRequest.splitAt(MaxReceiptsPerRequest)
          receiptsToRequest = remainingReceiptsToRequest
          receiptsRequested = currentReceiptsToRequest
          peerToRequest.ref ! SendMessage(GetReceipts(currentReceiptsToRequest))
        }
      }
    }
  }

  private def anyRequestsRemaining(): Boolean =
    nodesToRequest.nonEmpty || blockBodiesToRequest.nonEmpty || receiptsToRequest.nonEmpty || (blockHeaderToRequest < maxBlocks)

  private def dumpChainToFile(): Unit = {
    val headersFile = new FileWriter("headers.txt", true)
    val stateTreeFile = new FileWriter("stateTree.txt", true)
    val contractTreesFile = new FileWriter("contractTrees.txt", true)
    val evmCodeFile = new FileWriter("evmCode.txt", true)
    val receiptsFile = new FileWriter("receipts.txt", true)
    val bodiesFile = new FileWriter("bodies.txt", true)

    def dumpToFile(fw: FileWriter, element: (ByteString, ByteString)): Unit = element match {
      case (h, v) => fw.write(s"${Hex.toHexString(h.toArray[Byte])} ${Hex.toHexString(v.toArray)}\n")
    }

    blockHeadersStorage.foreach { case (headerHash, header) => dumpToFile(headersFile, headerHash -> header.toBytes) }
    stateStorage.foreach(s => dumpToFile(stateTreeFile, s._1 -> s._2.toBytes))
    contractStorage.foreach(c => dumpToFile(contractTreesFile, c._1 -> c._2.toBytes))
    evmCodeStorage.foreach { case (h, v) =>
      evmCodeFile.write(s"${Hex.toHexString(h.toArray[Byte])} ${Hex.toHexString(v.toArray[Byte])}\n")
    }
    blockReceiptsStorage.foreach { case (h, v) =>
      receiptsFile.write(s"${Hex.toHexString(h.toArray[Byte])} ${Hex.toHexString(v.toBytes)}\n")
    }
    blockBodyStorage.foreach { case (h, v) =>
      bodiesFile.write(s"${Hex.toHexString(h.toArray[Byte])} ${Hex.toHexString(v.toBytes)}\n")
    }

    headersFile.close()
    stateTreeFile.close()
    contractTreesFile.close()
    evmCodeFile.close()
    receiptsFile.close()
    bodiesFile.close()
  }
}

object DumpChainActor {
  val MaxHeadersPerRequest = 128
  val MaxBodiesPerRequest = 64
  val MaxNodesPerRequest = 128
  val MaxReceiptsPerRequest = 128

  def props(
      peerManager: ActorRef,
      peerMessageBus: ActorRef,
      startBlock: BigInt,
      maxBlocks: BigInt,
      bootstrapNode: String
  ): Props =
    Props(
      new DumpChainActor(peerManager, peerMessageBus: ActorRef, startBlock: BigInt, maxBlocks: BigInt, bootstrapNode)
    )
  val emptyStorage = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"))
  val emptyEvm = ByteString(Hex.decode("c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))
}
