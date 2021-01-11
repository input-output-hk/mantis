package io.iohk.ethereum.blockchain.sync.fast

import java.util.Comparator

import akka.util.ByteString
import com.google.common.hash.{BloomFilter, Funnel, PrimitiveSink}
import io.iohk.ethereum.blockchain.sync.fast.LoadableBloomFilter.BloomFilterLoadingResult
import io.iohk.ethereum.blockchain.sync.fast.SyncStateScheduler._
import io.iohk.ethereum.domain.{Account, Blockchain}
import io.iohk.ethereum.mpt.{BranchNode, ExtensionNode, HashNode, LeafNode, MerklePatriciaTrie, MptNode}
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders.MptNodeDec
import io.vavr.collection.PriorityQueue
import monix.eval.Task

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.util.Try

/**
  * Scheduler which traverses Merkle patricia trie in DFS fashion, while also creating requests for nodes missing in traversed
  * trie.
  * Traversal example: Merkle Patricia Trie with 2 leaf child nodes, each with non empty code value.
  * Final State:
  * BranchNode(hash: 1)
  * /                 \
  * Leaf(hash:2, codeHash:3)       Leaf(hash:4, codeHash:5)
  *
  * InitialState:
  * At initial state there is only: (hash: 1)
  *
  * Traversal in node by node fashion:
  * 1. Ask for root. After receive: (NodesToGet:[Hash:2, Hash4], nodesToSave: [])
  * 2. Ask for (Hash:2). After receive: (NodesToGet:[CodeHash:3, Hash4], nodesToSave: [])
  * 3. Ask for (CodeHash:3). After receive: (NodesToGet:[Hash:4], nodesToSave: [Leaf(hash:2, codeHash:3)])
  * 4. Ask for (Hash:4). After receive: (NodesToGet:[codeHash:5], nodesToSave: [Leaf(hash:2, codeHash:3)])
  * 5. Ask for (CodeHash:5).After receive:
  * (NodesToGet:[], nodesToSave: [Leaf(hash:2, codeHash:3)], Leaf(hash:4, codeHash:5),  BranchNode(hash: 1))
  *
  * BranchNode is only committed to save when all of its leaf nodes are retrieved, and all children of those leaf nodes i.e
  * storage and code are retrieved.
  *
  * SyncStateScheduler is agnostic to the way how SchedulerState is handled, it can be kept in var in actor, or in cats.Ref.
  *
  * Important part is that nodes retrieved by getMissingNodes, must eventually be provided for scheduler to make progress
  */
class SyncStateScheduler(blockchain: Blockchain, bloomFilter: LoadableBloomFilter[ByteString]) {

  val loadFilterFromBlockchain: Task[BloomFilterLoadingResult] = bloomFilter.loadFromSource

  def initState(targetRootHash: ByteString): Option[SchedulerState] = {
    if (targetRootHash == emptyStateRootHash) {
      None
    } else if (blockchain.getMptNodeByHash(targetRootHash).isDefined) {
      None
    } else {
      val initialState = SchedulerState()
      val initialRequest = StateNodeRequest(targetRootHash, None, StateNode, Seq(), 0, 0)
      Option(initialState.schedule(initialRequest))
    }
  }

  /**
    * Default responses processor which ignores duplicated or not requested hashes, but informs the caller about critical
    * errors.
    * If it would valuable, it possible to implement processor which would gather statistics about duplicated or not requested data.
    */
  def processResponses(
      state: SchedulerState,
      responses: List[SyncResponse]
  ): Either[CriticalError, (SchedulerState, ProcessingStatistics)] = {
    @tailrec
    def go(
        currentState: SchedulerState,
        currentStatistics: ProcessingStatistics,
        remaining: Seq[SyncResponse]
    ): Either[CriticalError, (SchedulerState, ProcessingStatistics)] = {
      if (remaining.isEmpty) {
        Right((currentState, currentStatistics))
      } else {
        val responseToProcess = remaining.head
        processResponse(currentState, responseToProcess) match {
          case Left(value) =>
            value match {
              case error: CriticalError =>
                Left(error)
              case err: NotCriticalError =>
                err match {
                  case SyncStateScheduler.NotRequestedItem =>
                    go(currentState, currentStatistics.addNotRequested(), remaining.tail)
                  case SyncStateScheduler.AlreadyProcessedItem =>
                    go(currentState, currentStatistics.addDuplicated(), remaining.tail)
                }
            }

          case Right(newState) =>
            go(newState, currentStatistics, remaining.tail)
        }
      }
    }

    go(state, ProcessingStatistics(), responses)
  }

  def getMissingNodes(state: SchedulerState, max: Int): (List[ByteString], SchedulerState) = {
    state.getMissingHashes(max)
  }

  def getAllMissingNodes(state: SchedulerState): (List[ByteString], SchedulerState) = {
    getMissingNodes(state, state.numberOfMissingHashes)
  }

  def persistBatch(state: SchedulerState, targetBlockNumber: BigInt): SchedulerState = {
    // Potential optimisation would be to expose some kind batch api from db to make only 1 write instead od 100k
    // for we could do this over code as it exposes DataSourceBatchUpdate, but not for mpt node as it write path is more
    // complex due to pruning.
    val (nodes, newState) = state.getNodesToPersist
    nodes.foreach { case (hash, (data, reqType)) =>
      bloomFilter.put(hash)
      reqType match {
        case _: CodeRequest =>
          blockchain.storeEvmCode(hash, data).commit()
        case _: NodeRequest =>
          blockchain.saveNode(hash, data.toArray, targetBlockNumber)
      }
    }
    newState
  }

  private def isRequestAlreadyKnownOrResolved(
      state: SchedulerState,
      response: SyncResponse
  ): Either[ResponseProcessingError, StateNodeRequest] = {
    for {
      activeRequest <- state.getPendingRequestByHash(response.hash).toRight(NotRequestedItem)
      _ <- if (activeRequest.resolvedData.isDefined) Left(AlreadyProcessedItem) else Right(())
    } yield activeRequest
  }

  private def processActiveResponse(
      state: SchedulerState,
      activeRequest: StateNodeRequest,
      response: SyncResponse
  ): Either[ResponseProcessingError, SchedulerState] = {
    activeRequest.requestType match {
      case _: CodeRequest => Right(state.commit(activeRequest.copy(resolvedData = Some(response.data))))
      case requestType: NodeRequest =>
        for {
          mptNode <- Try(response.data.toArray.toMptNode).toEither.left.map(_ => CannotDecodeMptNode)
          possibleChildRequests <- createPossibleChildRequests(mptNode, activeRequest, requestType)
        } yield {
          val childWithoutAlreadyKnown =
            possibleChildRequests.filterNot(req => isRequestedHashAlreadyCommitted(state, req))
          if (childWithoutAlreadyKnown.isEmpty && activeRequest.dependencies == 0) {
            state.commit(activeRequest.copy(resolvedData = Some(response.data)))
          } else {
            state.resolveRequest(activeRequest, response.data, childWithoutAlreadyKnown)
          }
        }
    }
  }

  def processResponse(
      state: SchedulerState,
      response: SyncResponse
  ): Either[ResponseProcessingError, SchedulerState] = {
    for {
      activeRequest <- isRequestAlreadyKnownOrResolved(state, response)
      newState <- processActiveResponse(state, activeRequest, response)
    } yield newState

  }
  // scalastyle:off method.length
  private def createPossibleChildRequests(
      mptNode: MptNode,
      parentRequest: StateNodeRequest,
      requestType: NodeRequest
  ): Either[NotAccountLeafNode.type, Seq[StateNodeRequest]] = mptNode match {
    case n: LeafNode =>
      requestType match {
        case SyncStateScheduler.StateNode =>
          Account(n.value).toEither.left.map(_ => NotAccountLeafNode).map { account =>
            // We are scheduling both storage trie and code requests with highest priority to be sure that leaf nodes completed
            // as fast as possible
            val evmRequests = if (account.codeHash != emptyCodeHash) {
              Seq(StateNodeRequest(account.codeHash, None, Code, Seq(parentRequest.nodeHash), maxMptTrieDepth, 0))
            } else {
              Seq()
            }

            val storageRequests = if (account.storageRoot != emptyStateRootHash) {
              Seq(
                StateNodeRequest(
                  account.storageRoot,
                  None,
                  StorageNode,
                  Seq(parentRequest.nodeHash),
                  maxMptTrieDepth,
                  0
                )
              )
            } else {
              Seq()
            }

            evmRequests ++ storageRequests
          }

        case SyncStateScheduler.StorageNode =>
          Right(Seq())
      }

    case n: BranchNode =>
      val children = ArraySeq.unsafeWrapArray(n.children)
      Right(children.collect { case HashNode(childHash) =>
        StateNodeRequest(
          ByteString.fromArrayUnsafe(childHash),
          None,
          requestType,
          Seq(parentRequest.nodeHash),
          parentRequest.nodeDepth + 1,
          0
        )
      })

    case n: ExtensionNode =>
      Right(n.next match {
        case HashNode(hash) =>
          Seq(
            StateNodeRequest(
              ByteString(hash),
              None,
              requestType,
              Seq(parentRequest.nodeHash),
              parentRequest.nodeDepth + n.sharedKey.size,
              0
            )
          )
        case _ => Nil
      })
    case _ => Right(Nil)
  }

  private def isInDatabase(req: StateNodeRequest): Boolean = {
    req.requestType match {
      case request: CodeRequest =>
        blockchain.getEvmCodeByHash(req.nodeHash).isDefined
      case request: NodeRequest =>
        blockchain.getMptNodeByHash(req.nodeHash).isDefined
    }
  }

  private def isRequestedHashAlreadyCommitted(state: SchedulerState, req: StateNodeRequest): Boolean = {
    state.memBatch.contains(req.nodeHash) ||
    (bloomFilter.mightContain(req.nodeHash) && isInDatabase(
      req
    )) // if hash is in bloom filter we need to double check on db
  }
}

object SyncStateScheduler {
  private val emptyStateRootHash = ByteString(MerklePatriciaTrie.EmptyRootHash)
  private val emptyCodeHash = Account.EmptyCodeHash
  private val maxMptTrieDepth = 64

  sealed abstract class RequestType

  sealed abstract class CodeRequest extends RequestType

  case object Code extends CodeRequest

  sealed abstract class NodeRequest extends RequestType

  case object StateNode extends NodeRequest

  case object StorageNode extends NodeRequest

  implicit object ByteStringFunnel extends Funnel[ByteString] {
    override def funnel(from: ByteString, into: PrimitiveSink): Unit = {
      into.putBytes(from.toArray)
    }
  }

  def getEmptyFilter(expectedFilterSize: Int): BloomFilter[ByteString] = {
    BloomFilter.create[ByteString](ByteStringFunnel, expectedFilterSize)
  }

  def apply(blockchain: Blockchain, expectedBloomFilterSize: Int): SyncStateScheduler = {
    // provided source i.e mptStateSavedKeys() is guaranteed to finish on first `Left` element which means that returned
    // error is the reason why loading has stopped
    new SyncStateScheduler(
      blockchain,
      LoadableBloomFilter[ByteString](expectedBloomFilterSize, blockchain.mptStateSavedKeys())
    )
  }

  final case class StateNodeRequest(
      nodeHash: ByteString,
      resolvedData: Option[ByteString],
      requestType: RequestType,
      parents: Seq[ByteString],
      nodeDepth: Int,
      dependencies: Int
  ) {
    def isNodeRequest: Boolean = requestType match {
      case _: CodeRequest => false
      case _: NodeRequest => true
    }
  }

  private val stateNodeRequestComparator = new Comparator[StateNodeRequest] {
    override def compare(o1: StateNodeRequest, o2: StateNodeRequest): Int = {
      o2.nodeDepth compare o1.nodeDepth
    }
  }

  implicit class Tuple2Ops[A, B](o: io.vavr.Tuple2[A, B]) {
    def asScala(): (A, B) = (o._1, o._2)
  }

  final case class SyncResponse(hash: ByteString, data: ByteString)

  case class SchedulerState(
      activeRequest: Map[ByteString, StateNodeRequest],
      queue: PriorityQueue[StateNodeRequest],
      memBatch: Map[ByteString, (ByteString, RequestType)]
  ) {

    def schedule(request: StateNodeRequest): SchedulerState = {
      activeRequest.get(request.nodeHash) match {
        case Some(oldRequest) =>
          copy(activeRequest + (request.nodeHash -> oldRequest.copy(parents = oldRequest.parents ++ request.parents)))

        case None =>
          copy(activeRequest + (request.nodeHash -> request), queue.enqueue(request))
      }
    }

    def getMissingHashes(max: Int): (List[ByteString], SchedulerState) = {
      @tailrec
      def go(
          currentQueue: PriorityQueue[StateNodeRequest],
          remaining: Int,
          got: List[ByteString]
      ): (PriorityQueue[StateNodeRequest], List[ByteString]) = {
        if (remaining == 0) {
          (currentQueue, got.reverse)
        } else if (currentQueue.isEmpty) {
          (currentQueue, got.reverse)
        } else {
          val (elem, newQueue) = currentQueue.dequeue().asScala()
          go(newQueue, remaining - 1, elem.nodeHash :: got)
        }
      }

      val (newQueue, elements) = go(queue, max, List.empty)
      (elements, copy(queue = newQueue))
    }

    def getAllMissingHashes: (List[ByteString], SchedulerState) = getMissingHashes(queue.size())

    def numberOfPendingRequests: Int = activeRequest.size

    def getPendingRequestByHash(hash: ByteString): Option[StateNodeRequest] = activeRequest.get(hash)

    def numberOfMissingHashes: Int = queue.size()

    def commit(request: StateNodeRequest): SchedulerState = {
      @tailrec
      def go(
          currentRequests: Map[ByteString, StateNodeRequest],
          currentBatch: Map[ByteString, (ByteString, RequestType)],
          parentsToCheck: Seq[ByteString]
      ): (Map[ByteString, StateNodeRequest], Map[ByteString, (ByteString, RequestType)]) = {
        if (parentsToCheck.isEmpty) {
          (currentRequests, currentBatch)
        } else {
          val parent = parentsToCheck.head
          // if the parent is not there, something is terribly wrong and our assumptions do not hold, it is perfectly fine to
          // fail with exception
          val parentRequest = currentRequests.getOrElse(
            parent,
            throw new IllegalStateException(
              "Critical error. Missing parent " +
                s"with hash ${parent}"
            )
          )
          val newParentDeps = parentRequest.dependencies - 1
          if (newParentDeps == 0) {
            // we can always call `parentRequest.resolvedData.get` on parent node, as to even have children parent data
            // needs to be provided
            go(
              currentRequests - parent,
              currentBatch + (parent -> (parentRequest.resolvedData.getOrElse(
                throw new IllegalStateException(
                  s"Critical error. Parent ${parentRequest.nodeHash} without resolved data"
                )
              ), parentRequest.requestType)),
              parentsToCheck.tail ++ parentRequest.parents
            )
          } else {
            go(
              currentRequests + (parent -> parentRequest.copy(dependencies = newParentDeps)),
              currentBatch,
              parentsToCheck.tail
            )
          }
        }
      }

      val newActive = activeRequest - request.nodeHash
      val newMemBatch = memBatch + (request.nodeHash -> (request.resolvedData.get, request.requestType))

      val (newRequests, newBatch) = go(newActive, newMemBatch, request.parents)
      copy(activeRequest = newRequests, memBatch = newBatch)
    }

    def resolveRequest(
        request: StateNodeRequest,
        receivedData: ByteString,
        requestNewChildren: Seq[StateNodeRequest]
    ): SchedulerState = {
      val numberOfChildren = requestNewChildren.size
      val resolvedStateNodeRequest =
        request.copy(resolvedData = Some(receivedData), dependencies = request.dependencies + numberOfChildren)
      val newRequests = activeRequest + (request.nodeHash -> resolvedStateNodeRequest)
      val stateWithUpdatedParent = copy(activeRequest = newRequests)
      requestNewChildren.foldLeft(stateWithUpdatedParent) { case (state, child) => state.schedule(child) }
    }

    def getNodesToPersist: (Seq[(ByteString, (ByteString, RequestType))], SchedulerState) = {
      (memBatch.toSeq, copy(memBatch = Map.empty))
    }
  }

  object SchedulerState {
    def apply(): SchedulerState = {
      new SchedulerState(
        Map.empty[ByteString, StateNodeRequest],
        PriorityQueue.empty(stateNodeRequestComparator),
        Map.empty
      )
    }
  }

  case object ProcessingSuccess

  sealed trait ResponseProcessingError

  sealed trait CriticalError extends ResponseProcessingError

  case object CannotDecodeMptNode extends CriticalError

  case object NotAccountLeafNode extends CriticalError

  sealed trait NotCriticalError extends ResponseProcessingError

  case object NotRequestedItem extends NotCriticalError

  case object AlreadyProcessedItem extends NotCriticalError

  final case class ProcessingStatistics(duplicatedHashes: Long, notRequestedHashes: Long, saved: Long) {
    def addNotRequested(): ProcessingStatistics = copy(notRequestedHashes = notRequestedHashes + 1)
    def addDuplicated(): ProcessingStatistics = copy(duplicatedHashes = duplicatedHashes + 1)
    def addSaved(newSaved: Long): ProcessingStatistics = copy(saved = saved + newSaved)
    def addStats(that: ProcessingStatistics): ProcessingStatistics =
      copy(
        duplicatedHashes = that.duplicatedHashes,
        notRequestedHashes = that.notRequestedHashes
      )
  }

  object ProcessingStatistics {
    def apply(): ProcessingStatistics = new ProcessingStatistics(0, 0, 0)
  }
}
