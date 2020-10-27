package io.iohk.ethereum.blockchain.sync.regular

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.testkit.TestActor.AutoPilot
import akka.testkit.{TestKitBase, TestProbe}
import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import cats.Eq
import cats.instances.list._
import cats.syntax.functor._
import io.iohk.ethereum.{Fixtures, ObjectGenerators}
import io.iohk.ethereum.blockchain.sync.PeerListSupport.PeersMap
import io.iohk.ethereum.blockchain.sync.{EphemBlockchainTestSetup, PeersClient, TestSyncConfig}
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.domain._
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.Subscribe
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.network.{Peer, PeerId}
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.scalamock.scalatest.MockFactory

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigInt
import scala.reflect.ClassTag
import org.scalatest.matchers.should.Matchers

// Fixture classes are wrapped in a trait due to problems with making mocks available inside of them
trait RegularSyncFixtures { self: Matchers with MockFactory =>
  abstract class RegularSyncFixture(_system: ActorSystem)
      extends TestKitBase
      with EphemBlockchainTestSetup
      with TestSyncConfig
      with SecureRandomBuilder {
    implicit override lazy val system: ActorSystem = _system
    override lazy val syncConfig: SyncConfig =
      defaultSyncConfig.copy(blockHeadersPerRequest = 2, blockBodiesPerRequest = 2)
    val handshakedPeers: PeersMap = (0 to 5).toList.map((peerId _).andThen(getPeer)).fproduct(getPeerInfo).toMap
    val defaultPeer: Peer = peerByNumber(0)

    val etcPeerManager: TestProbe = TestProbe()
    val peerEventBus: TestProbe = TestProbe()
    val ommersPool: TestProbe = TestProbe()
    val pendingTransactionsManager: TestProbe = TestProbe()
    val checkpointBlockGenerator: CheckpointBlockGenerator = new CheckpointBlockGenerator()
    val peersClient: TestProbe = TestProbe()

    val regularSync: ActorRef = system.actorOf(
      RegularSync
        .props(
          peersClient.ref,
          etcPeerManager.ref,
          peerEventBus.ref,
          ledger,
          blockchain,
          syncConfig,
          ommersPool.ref,
          pendingTransactionsManager.ref,
          checkpointBlockGenerator,
          system.scheduler
        )
        .withDispatcher("akka.actor.default-dispatcher")
    )

    // scalastyle:off magic.number
    val defaultHeader = Fixtures.Blocks.ValidBlock.header.copy(
      difficulty = 1000000,
      number = 1,
      gasLimit = 1000000,
      gasUsed = 0,
      unixTimestamp = 0
    )

    val defaultTx = Transaction(
      nonce = 42,
      gasPrice = 1,
      gasLimit = 90000,
      receivingAddress = Address(123),
      value = 0,
      payload = bEmpty
    )

    val defaultTd = 12345

    val keyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)

    val genesis: Block = Block(defaultHeader.copy(number = 0), BlockBody(Nil, Nil))
    val testBlocks: List[Block] = getBlocks(20, genesis)
    val testBlocksChunked: List[List[Block]] = testBlocks.grouped(syncConfig.blockHeadersPerRequest).toList

    override lazy val ledger = new TestLedgerImpl

    blockchain.save(block = genesis, receipts = Nil, totalDifficulty = BigInt(10000), saveAsBestBlock = true)
    // scalastyle:on magic.number

    def done(): Unit =
      regularSync ! PoisonPill

    def randomHash(): ByteString =
      ObjectGenerators.byteStringOfLengthNGen(32).sample.get

    def getBlocks(amount: Int, parent: Block): List[Block] =
      (1 to amount).toList.foldLeft[List[Block]](Nil)((generated, _) => {
        val theParent = generated.lastOption.getOrElse(parent)
        generated :+ getBlock(theParent)
      })

    def getBlock(nr: BigInt, parent: Block): Block = {
      val header = defaultHeader.copy(extraData = randomHash(), number = nr, parentHash = parent.hash)
      val ommer = defaultHeader.copy(extraData = randomHash())
      val tx = defaultTx.copy(payload = randomHash())
      val stx = SignedTransaction.sign(tx, keyPair, None)

      Block(header, BlockBody(List(stx.tx), List(ommer)))
    }
    def getBlock(parent: Block): Block = getBlock(parent.number + 1, parent)

    def peerId(number: Int): PeerId = PeerId(s"peer_$number")

    def getPeer(id: PeerId): Peer =
      Peer(new InetSocketAddress("127.0.0.1", 0), TestProbe(id.value).ref, incomingConnection = false)

    def getPeerInfo(peer: Peer): PeerInfo = {
      val status = Status(1, 1, 1, ByteString(s"${peer.id}_bestHash"), ByteString("unused"))
      PeerInfo(
        status,
        forkAccepted = true,
        totalDifficulty = status.totalDifficulty,
        maxBlockNumber = 0,
        bestBlockHash = status.bestHash
      )
    }

    def peerByNumber(number: Int): Peer = handshakedPeers.keys.toList.sortBy(_.id.value).apply(number)

    def blockHeadersRequest(fromChunk: Int): PeersClient.Request[GetBlockHeaders] = PeersClient.Request.create(
      GetBlockHeaders(
        Left(testBlocksChunked(fromChunk).headNumberUnsafe),
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

    class TestLedgerImpl extends LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus, system.dispatcher) {
      protected val results = mutable.Map[ByteString, () => Future[BlockImportResult]]()
      protected val importedBlocks = mutable.Set[Block]()

      override def importBlock(
          block: Block
      )(implicit blockExecutionContext: ExecutionContext): Future[BlockImportResult] = {
        importedBlocks.add(block)
        results(block.hash)()
      }

      override def getBlockByHash(hash: ByteString): Option[Block] =
        importedBlocks.find(_.hash == hash)

      def setImportResult(block: Block, result: () => Future[BlockImportResult]): Unit =
        results(block.header.hash) = result

      def didTryToImportBlock(predicate: Block => Boolean): Boolean =
        importedBlocks.exists(predicate)

      def didTryToImportBlock(block: Block): Boolean =
        didTryToImportBlock(_.hash == block.hash)

      def bestBlock: Block = importedBlocks.maxBy(_.number)
    }

    class PeersClientAutoPilot(blocks: List[Block] = testBlocks) extends AutoPilot {

      def run(sender: ActorRef, msg: Any): AutoPilot =
        overrides(sender).orElse(defaultHandlers(sender)).apply(msg).getOrElse(defaultAutoPilot)

      def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = PartialFunction.empty

      def defaultHandlers(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
        case PeersClient.Request(GetBlockHeaders(Left(minBlock), amount, _, _), _, _) =>
          val maxBlock = minBlock + amount
          val matchingHeaders = blocks
            .filter(b => {
              val nr = b.number
              minBlock <= nr && nr < maxBlock
            })
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
        assert(Eq[T].eqv(received, msg))
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
  }

  abstract class OnTopFixture(system: ActorSystem) extends RegularSyncFixture(system) {

    val newBlock: Block = getBlock(testBlocks.last)

    override lazy val ledger: TestLedgerImpl = stub[TestLedgerImpl]

    var blockFetcher: ActorRef = _

    var importedNewBlock = false
    var importedLastTestBlock = false
    (ledger.resolveBranch _).when(*).returns(NewBetterBranch(Nil))
    (ledger
      .importBlock(_: Block)(_: ExecutionContext))
      .when(*, *)
      .onCall((block, _) => {
        if (block == newBlock) {
          importedNewBlock = true
          Future.successful(BlockImportedToTop(List(BlockData(newBlock, Nil, newBlock.number))))
        } else {
          if (block == testBlocks.last) {
            importedLastTestBlock = true
          }
          Future.successful(BlockImportedToTop(Nil))
        }
      })

    peersClient.setAutoPilot(new PeersClientAutoPilot)

    def waitForSubscription(): Unit = {
      peerEventBus.expectMsgClass(classOf[Subscribe])
      blockFetcher = peerEventBus.sender()
    }

    def sendLastTestBlockAsTop(): Unit = sendNewBlock(testBlocks.last)

    def sendNewBlock(block: Block = newBlock, peer: Peer = defaultPeer): Unit =
      blockFetcher ! MessageFromPeer(NewBlock(block, block.number), peer.id)

    def goToTop(): Unit = {
      regularSync ! RegularSync.Start

      waitForSubscription()
      sendLastTestBlockAsTop()

      awaitCond(importedLastTestBlock)
    }
  }
}
