package io.iohk.ethereum.blockchain.sync.regular

import java.net.InetSocketAddress

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.PoisonPill
import akka.pattern.ask
import akka.testkit.TestActor.AutoPilot
import akka.testkit.TestKitBase
import akka.testkit.TestProbe
import akka.util.ByteString
import akka.util.Timeout

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._

import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ReplaySubject

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import scala.math.BigInt
import scala.reflect.ClassTag

import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.blockchain.sync._
import io.iohk.ethereum.consensus.ConsensusAdapter
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.Subscribe
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.ETC64.NewBlock
import io.iohk.ethereum.network.p2p.messages.ETH62._
import io.iohk.ethereum.network.p2p.messages.ETH63.GetNodeData
import io.iohk.ethereum.network.p2p.messages.ETH63.NodeData
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config.SyncConfig

// Fixture classes are wrapped in a trait due to problems with making mocks available inside of them
trait RegularSyncFixtures { self: Matchers with AsyncMockFactory =>
  class RegularSyncFixture(_system: ActorSystem)
      extends TestKitBase
      with EphemBlockchainTestSetup
      with TestSyncConfig
      with SecureRandomBuilder {
    implicit lazy val timeout: Timeout = remainingOrDefault
    implicit override lazy val system: ActorSystem = _system
    override lazy val syncConfig: SyncConfig =
      defaultSyncConfig.copy(blockHeadersPerRequest = 2, blockBodiesPerRequest = 2)
    val handshakedPeers: Map[Peer, PeerInfo] =
      (0 to 5).toList.map((peerId _).andThen(getPeer)).fproduct(getPeerInfo(_)).toMap
    val defaultPeer: Peer = peerByNumber(0)

    val etcPeerManager: TestProbe = TestProbe()
    val peerEventBus: TestProbe = TestProbe()
    val ommersPool: TestProbe = TestProbe()
    val pendingTransactionsManager: TestProbe = TestProbe()
    val checkpointBlockGenerator: CheckpointBlockGenerator = new CheckpointBlockGenerator()
    val peersClient: TestProbe = TestProbe()
    val blacklist: CacheBasedBlacklist = CacheBasedBlacklist.empty(100)
    lazy val branchResolution = new BranchResolution(blockchainReader)

    val stateStorage: StateStorage = stub[StateStorage]

    lazy val regularSync: ActorRef = system.actorOf(
      RegularSync
        .props(
          peersClient.ref,
          etcPeerManager.ref,
          peerEventBus.ref,
          consensusAdapter,
          blockchainReader,
          stateStorage,
          branchResolution,
          validators.blockValidator,
          blacklist,
          syncConfig,
          ommersPool.ref,
          pendingTransactionsManager.ref,
          system.scheduler,
          this,
          newFlow = false
        )
        .withDispatcher("akka.actor.default-dispatcher")
    )

    val defaultTd = 12345

    val testBlocks: List[Block] = BlockHelpers.generateChain(20, BlockHelpers.genesis)
    val testBlocksChunked: List[List[Block]] = testBlocks.grouped(syncConfig.blockHeadersPerRequest).toList

    override lazy val consensusAdapter: ConsensusAdapter = {
      val adapter = stub[ConsensusAdapter]
      (adapter
        .evaluateBranchBlock(_: Block)(_: Scheduler, _: BlockchainConfig))
        .when(*, *, *)
        .onCall { case (block: Block, _, _) =>
          importedBlocksSet.add(block)
          results(block.header.hash).flatTap(_ => Task.fromFuture(importedBlocksSubject.onNext(block)))
        }
      adapter
    }

    blockchainWriter.save(
      block = BlockHelpers.genesis,
      receipts = Nil,
      weight = ChainWeight.totalDifficultyOnly(10000),
      saveAsBestBlock = true
    )
    // scalastyle:on magic.number

    def done(): Unit =
      regularSync ! PoisonPill

    def peerId(number: Int): PeerId = PeerId(s"peer_$number")

    def getPeer(id: PeerId): Peer =
      Peer(id, new InetSocketAddress("127.0.0.1", 0), TestProbe(id.value).ref, incomingConnection = false)

    def getPeerInfo(
        peer: Peer,
        capability: Capability = Capability.ETC64
    ): PeerInfo = {
      val status =
        RemoteStatus(
          capability,
          1,
          ChainWeight.totalDifficultyOnly(1),
          ByteString(s"${peer.id}_bestHash"),
          ByteString("unused")
        )
      PeerInfo(
        status,
        forkAccepted = true,
        chainWeight = status.chainWeight,
        maxBlockNumber = 0,
        bestBlockHash = status.bestHash
      )
    }

    def peerByNumber(number: Int): Peer = handshakedPeers.keys.toList.sortBy(_.id.value).apply(number)

    def blockHeadersChunkRequest(fromChunk: Int): PeersClient.Request[GetBlockHeaders] = {
      val block = testBlocksChunked(fromChunk).headNumberUnsafe
      blockHeadersRequest(block)
    }

    def blockHeadersRequest(fromBlock: BigInt): PeersClient.Request[GetBlockHeaders] = PeersClient.Request.create(
      GetBlockHeaders(
        Left(fromBlock),
        syncConfig.blockHeadersPerRequest,
        skip = 0,
        reverse = false
      ),
      PeersClient.BestPeer
    )

    def fishForBlacklistPeer(peer: Peer): PeersClient.BlacklistPeer =
      peersClient.fishForSpecificMessage() {
        case msg @ PeersClient.BlacklistPeer(id, _) if id == peer.id => msg
      }

    val getSyncStatus: Task[SyncProtocol.Status] =
      Task.deferFuture((regularSync ? SyncProtocol.GetStatus).mapTo[SyncProtocol.Status])

    def pollForStatus(predicate: SyncProtocol.Status => Boolean): Task[SyncProtocol.Status] = Observable
      .repeatEvalF(getSyncStatus.delayExecution(10.millis))
      .takeWhileInclusive(predicate.andThen(!_))
      .lastL
      .timeout(remainingOrDefault)

    def fishForStatus[B](picker: PartialFunction[SyncProtocol.Status, B]): Task[B] = Observable
      .repeatEvalF(getSyncStatus.delayExecution(10.millis))
      .collect(picker)
      .firstL
      .timeout(remainingOrDefault)

    protected val results: mutable.Map[ByteString, Task[BlockImportResult]] =
      mutable.Map[ByteString, Task[BlockImportResult]]()
    protected val importedBlocksSet: mutable.Set[Block] = mutable.Set[Block]()
    private val importedBlocksSubject = ReplaySubject[Block]()
    val importedBlocks: Observable[Block] = importedBlocksSubject

    def didTryToImportBlock(predicate: Block => Boolean): Boolean =
      importedBlocksSet.exists(predicate)

    def didTryToImportBlock(block: Block): Boolean =
      didTryToImportBlock(_.hash == block.hash)

    def bestBlock: Block = importedBlocksSet.maxBy(_.number)

    def setImportResult(block: Block, result: Task[BlockImportResult]): Unit =
      results(block.header.hash) = result

    class PeersClientAutoPilot(blocks: List[Block] = testBlocks) extends AutoPilot {

      def run(sender: ActorRef, msg: Any): AutoPilot =
        overrides(sender).orElse(defaultHandlers(sender)).apply(msg).getOrElse(defaultAutoPilot)

      def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = PartialFunction.empty

      def defaultHandlers(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
        case PeersClient.Request(GetBlockHeaders(Left(minBlock), amount, _, _), _, _) =>
          val maxBlock = minBlock + amount
          val matchingHeaders = blocks
            .filter { b =>
              val nr = b.number
              minBlock <= nr && nr < maxBlock
            }
            .map(_.header)
            .sortBy(_.number)
          sender ! PeersClient.Response(defaultPeer, BlockHeaders(matchingHeaders))
          None
        case PeersClient.Request(GetBlockBodies(hashes), _, _) =>
          val matchingBodies = hashes.flatMap(hash => blocks.find(_.hash == hash)).map(_.body)

          sender ! PeersClient.Response(defaultPeer, BlockBodies(matchingBodies))
          None
        case PeersClient.Request(GetNodeData(hash :: Nil), _, _) =>
          sender ! PeersClient.Response(
            defaultPeer,
            NodeData(List(ByteString(blocks.byHashUnsafe(hash).header.toBytes: Array[Byte])))
          )
          None
        case _ => None
      }

      def defaultAutoPilot: AutoPilot = this
    }

    implicit class ListOps[T](list: List[T]) {

      def get(index: Int): Option[T] =
        if (list.isDefinedAt(index)) {
          Some(list(index))
        } else {
          None
        }
    }

    // TODO: consider extracting it somewhere closer to domain
    implicit class BlocksListOps(blocks: List[Block]) {
      def headNumberUnsafe: BigInt = blocks.head.number
      def headNumber: Option[BigInt] = blocks.headOption.map(_.number)
      def headers: List[BlockHeader] = blocks.map(_.header)
      def hashes: List[ByteString] = headers.map(_.hash)
      def bodies: List[BlockBody] = blocks.map(_.body)
      def numbers: List[BigInt] = blocks.map(_.number)
      def numberAt(index: Int): Option[BigInt] = blocks.get(index).map(_.number)
      def numberAtUnsafe(index: Int): BigInt = numberAt(index).get
      def byHash(hash: ByteString): Option[Block] = blocks.find(_.hash == hash)
      def byHashUnsafe(hash: ByteString): Block = byHash(hash).get
    }

    // TODO: consider extracting it into common test environment
    implicit class TestProbeOps(probe: TestProbe) {

      def expectMsgEq[T: Eq](msg: T): T = expectMsgEq(remainingOrDefault, msg)

      def expectMsgEq[T: Eq](max: FiniteDuration, msg: T): T = {
        val received = probe.expectMsgClass(max, msg.getClass)
        assert(Eq[T].eqv(received, msg), s"Expected ${msg}, got ${received}")
        received
      }

      def fishForSpecificMessageMatching[T](
          max: FiniteDuration = probe.remainingOrDefault
      )(predicate: Any => Boolean): T =
        probe.fishForSpecificMessage(max) {
          case msg if predicate(msg) => msg.asInstanceOf[T]
        }

      def fishForMsgEq[T: Eq: ClassTag](msg: T, max: FiniteDuration = probe.remainingOrDefault): T =
        probe.fishForSpecificMessageMatching[T](max)(x =>
          implicitly[ClassTag[T]].runtimeClass.isInstance(x) && Eq[T].eqv(msg, x.asInstanceOf[T])
        )

      def expectMsgAllOfEq[T1: Eq, T2: Eq](msg1: T1, msg2: T2): (T1, T2) =
        expectMsgAllOfEq(remainingOrDefault, msg1, msg2)

      def expectMsgAllOfEq[T1: Eq, T2: Eq](max: FiniteDuration, msg1: T1, msg2: T2): (T1, T2) = {
        val received = probe.receiveN(2, max)
        (
          received.find(m => Eq[T1].eqv(msg1, m.asInstanceOf[T1])).get.asInstanceOf[T1],
          received.find(m => Eq[T2].eqv(msg2, m.asInstanceOf[T2])).get.asInstanceOf[T2]
        )
      }
    }

    implicit def eqInstanceForPeersClientRequest[T <: Message]: Eq[PeersClient.Request[T]] =
      (x, y) => x.message == y.message && x.peerSelector == y.peerSelector

    def fakeEvaluateBlock(
        block: Block
    ): Task[BlockImportResult] = {
      val result: BlockImportResult = if (didTryToImportBlock(block)) {
        DuplicateBlock
      } else {
        if (importedBlocksSet.isEmpty || bestBlock.isParentOf(block) || importedBlocksSet.exists(_.isParentOf(block))) {
          importedBlocksSet.add(block)
          BlockImportedToTop(List(BlockData(block, Nil, ChainWeight.totalDifficultyOnly(block.header.difficulty))))
        } else if (block.number > bestBlock.number) {
          importedBlocksSet.add(block)
          BlockEnqueued
        } else {
          BlockImportFailed("foo")
        }
      }

      Task.now(result)
    }

    class FakeBranchResolution extends BranchResolution(stub[BlockchainReader]) {
      override def resolveBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult = {
        val importedHashes = importedBlocksSet.map(_.hash).toSet

        if (
          importedBlocksSet.isEmpty || (importedHashes.contains(
            headers.head.parentHash
          ) && headers.last.number > bestBlock.number)
        )
          NewBetterBranch(Nil)
        else
          UnknownBranch
      }
    }
  }

  class OnTopFixture(system: ActorSystem) extends RegularSyncFixture(system) {

    val newBlock: Block = BlockHelpers.generateBlock(testBlocks.last)

    override lazy val consensusAdapter: ConsensusAdapter = stub[ConsensusAdapter]

    var blockFetcher: ActorRef = _

    var importedNewBlock = false
    var importedLastTestBlock = false

    override lazy val branchResolution: BranchResolution = stub[BranchResolution]
    (branchResolution.resolveBranch _).when(*).returns(NewBetterBranch(Nil))

    (consensusAdapter
      .evaluateBranchBlock(_: Block)(_: Scheduler, _: BlockchainConfig))
      .when(*, *, *)
      .onCall { (block, _, _) =>
        if (block == newBlock) {
          importedNewBlock = true
          Task.now(
            BlockImportedToTop(List(BlockData(newBlock, Nil, ChainWeight(0, newBlock.number))))
          )
        } else {
          if (block == testBlocks.last) {
            importedLastTestBlock = true
          }
          Task.now(BlockImportedToTop(Nil))
        }
      }

    peersClient.setAutoPilot(new PeersClientAutoPilot)

    def waitForSubscription(): Unit = {
      peerEventBus.expectMsgClass(classOf[Subscribe])
      blockFetcher = peerEventBus.sender()
    }

    def sendLastTestBlockAsTop(): Unit = sendNewBlock(testBlocks.last)

    def sendNewBlock(block: Block = newBlock, peer: Peer = defaultPeer): Unit =
      blockFetcher ! MessageFromPeer(NewBlock(block, ChainWeight.totalDifficultyOnly(block.number)), peer.id)

    def goToTop(): Unit = {
      regularSync ! SyncProtocol.Start

      waitForSubscription()
      sendLastTestBlockAsTop()

      awaitCond(importedLastTestBlock)
    }
  }
}
