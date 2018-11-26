package io.iohk.ethereum.blockchain.sync.regular

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.testkit.TestActor.AutoPilot
import akka.testkit.{TestKit, TestKitBase, TestProbe}
import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import cats.Eq
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.blockchain.sync.PeerListSupport.PeersMap
import io.iohk.ethereum.blockchain.sync.{EphemBlockchainTestSetup, PeersClient, TestSyncConfig}
import io.iohk.ethereum.crypto.{generateKeyPair, kec256}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.network.EtcPeerManagerActor.{GetHandshakedPeers, HandshakedPeers, PeerInfo}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerEventBusActor, PeerId}
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.ommers.OmmersPool.{AddOmmers, RemoveOmmers}
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpecLike}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.math.BigInt

class RegularSyncSpec extends WordSpecLike with BeforeAndAfterEach with Matchers with MockFactory {
  var testSystem: ActorSystem = _

  override def beforeEach: Unit =
    testSystem = ActorSystem()

  override def afterEach: Unit =
    TestKit.shutdownActorSystem(testSystem)

  "Regular Sync" when {
    "initializing" should {
      "subscribe for new blocks and new hashes" in new Fixture(testSystem) {
        regularSync ! RegularSync.Start

        peerEventBus.expectMsg(
          PeerEventBusActor.Subscribe(
            MessageClassifier(Set(NewBlock.code, NewBlockHashes.code), PeerSelector.AllPeers)))
      }
      "subscribe to handshaked peers list" in new Fixture(testSystem) {
        etcPeerManager.expectMsg(EtcPeerManagerActor.GetHandshakedPeers)
      }
    }

    "fetching blocks" should {
      "fetch headers and bodies concurrently" in new Fixture(testSystem) {
        regularSync ! RegularSync.Start

        peerEventBus.expectMsgClass(classOf[Subscribe])
        peerEventBus.reply(MessageFromPeer(NewBlock(testBlocks.last, Block.number(testBlocks.last)), defaultPeer.id))

        peersClient.expectMsgEq(blockHeadersRequest(0))
        peersClient.reply(PeersClient.Response(defaultPeer, BlockHeaders(testBlocksChunked.head.headers)))
        peersClient.expectMsgAllOfEq(
          blockHeadersRequest(1),
          PeersClient.Request.create(GetBlockBodies(testBlocksChunked.head.hashes), PeersClient.BestPeer)
        )
      }

      "blacklist peer which caused failed request" in new Fixture(testSystem) {
        regularSync ! RegularSync.Start

        peersClient.expectMsgType[PeersClient.Request[GetBlockHeaders]]
        peersClient.reply(PeersClient.RequestFailed(defaultPeer, "a random reason"))
        peersClient.expectMsg(PeersClient.BlacklistPeer(defaultPeer.id, "a random reason"))
      }

      "blacklist peer which returns headers starting from one with higher number than expected" in new Fixture(
        testSystem) {
        regularSync ! RegularSync.Start

        peersClient.expectMsgEq(blockHeadersRequest(0))
        peersClient.reply(PeersClient.Response(defaultPeer, BlockHeaders(testBlocksChunked(1).headers)))
        peersClient.expectMsgPF() {
          case PeersClient.BlacklistPeer(id, _) if id == defaultPeer.id => true
        }
      }

      "blacklist peer which returns headers not forming a chain" in new Fixture(testSystem) {
        regularSync ! RegularSync.Start

        peersClient.expectMsgEq(blockHeadersRequest(0))
        peersClient.reply(
          PeersClient.Response(defaultPeer, BlockHeaders(testBlocksChunked.head.headers.filter(_.number % 2 == 0))))
        peersClient.expectMsgPF() {
          case PeersClient.BlacklistPeer(id, _) if id == defaultPeer.id => true
        }
      }

      "wait for time defined in config until issuing a retry request due to no suitable peer" in new Fixture(testSystem) {
        regularSync ! RegularSync.Start

        peersClient.expectMsgEq(blockHeadersRequest(0))
        peersClient.reply(PeersClient.NoSuitablePeer)
        peersClient.expectNoMessage(syncConfig.syncRetryInterval)
        peersClient.expectMsgEq(blockHeadersRequest(0))
      }

      "not fetch new blocks if fetcher's queue reached size defined in configuration" in new Fixture(testSystem) {
        override lazy val syncConfig: SyncConfig = defaultSyncConfig.copy(
          syncRetryInterval = testKitSettings.DefaultTimeout.duration,
          maxFetcherQueueSize = 1,
          blockBodiesPerRequest = 2,
          blockHeadersPerRequest = 2,
          blocksBatchSize = 2
        )

        regularSync ! RegularSync.Start

        peerEventBus.expectMsgClass(classOf[Subscribe])
        peerEventBus.reply(
          MessageFromPeer(NewBlock(testBlocks.last, testBlocks.last.header.difficulty), defaultPeer.id))

        peersClient.expectMsgEq(blockHeadersRequest(0))
        peersClient.reply(PeersClient.Response(defaultPeer, BlockHeaders(testBlocksChunked.head.headers)))
        peersClient.expectMsgEq(
          PeersClient.Request.create(GetBlockBodies(testBlocksChunked.head.hashes), PeersClient.BestPeer))
        peersClient.reply(PeersClient.Response(defaultPeer, BlockBodies(testBlocksChunked.head.bodies)))

        peersClient.expectNoMessage()
      }
    }

    "resolving branches" should {
      trait FakeLedger { self: Fixture =>
        class FakeLedgerImpl extends TestLedgerImpl {
          override def importBlock(block: Block)(
              implicit blockExecutionContext: ExecutionContext): Future[BlockImportResult] = {
            val result: BlockImportResult = if (didTryToImportBlock(block)) {
              DuplicateBlock
            } else {
              if (importedBlocks.isEmpty || Block.isParentOf(bestBlock, block)) {
                importedBlocks.add(block)
                BlockImportedToTop(List(BlockData(block, Nil, block.header.difficulty)))
              } else if (Block.number(block) > Block.number(bestBlock)) {
                importedBlocks.add(block)
                BlockEnqueued
              } else {
                BlockImportFailed("foo")
              }
            }

            Future.successful(result)
          }

          override def resolveBranch(headers: scala.Seq[BlockHeader]): BranchResolutionResult = {
            val importedHashes = importedBlocks.map(Block.hash).toSet

            if (importedBlocks.isEmpty || (importedHashes.contains(headers.head.parentHash) && headers.last.number > Block
                .number(bestBlock))) {
              NewBetterBranch(Nil)
            } else {
              UnknownBranch
            }
          }
        }
      }

      "go back to earlier block in order to find a common parent with new branch" in new Fixture(testSystem)
      with FakeLedger {
        implicit val ec: ExecutionContext = system.dispatcher
        override lazy val ledger: TestLedgerImpl = new FakeLedgerImpl()

        val commonPart: List[Block] = testBlocks.take(syncConfig.blocksBatchSize)
        val alternativeBranch: List[Block] = getBlocks(syncConfig.blocksBatchSize * 2, commonPart.last)
        val alternativeBlocks: List[Block] = commonPart ++ alternativeBranch

        class BranchResolutionAutoPilot(didResponseWithNewBranch: Boolean, blocks: List[Block])
            extends PeersClientAutoPilot(blocks) {
          override def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
            case PeersClient.Request(GetBlockHeaders(Left(nr), maxHeaders, _, _), _, _)
                if nr >= alternativeBranch.numberAtUnsafe(syncConfig.blocksBatchSize) && !didResponseWithNewBranch =>
              val responseHeaders = alternativeBranch.headers.filter(_.number >= nr).take(maxHeaders.toInt)
              sender ! PeersClient.Response(defaultPeer, BlockHeaders(responseHeaders))
              Some(new BranchResolutionAutoPilot(true, alternativeBlocks))
          }
        }

        peersClient.setAutoPilot(new BranchResolutionAutoPilot(didResponseWithNewBranch = false, testBlocks))

        Await.result(ledger.importBlock(genesis), remainingOrDefault)

        regularSync ! RegularSync.Start

        peerEventBus.expectMsgClass(classOf[Subscribe])
        peerEventBus.reply(MessageFromPeer(NewBlock(testBlocks.last, Block.number(testBlocks.last)), defaultPeer.id))

        awaitCond(ledger.bestBlock == alternativeBlocks.last, 15.seconds)
      }
    }

    "fetching state node" should {
      abstract class MissingStateNodeFixture(system: ActorSystem) extends Fixture(system) {
        val failingBlock: Block = testBlocksChunked.head.head
        ledger.setImportResult(failingBlock, () => Future.failed(new MissingNodeException(Block.hash(failingBlock))))
      }

      "blacklist peer which returns empty response" in new MissingStateNodeFixture(testSystem) {
        val failingPeer: Peer = peerByNumber(1)

        peersClient.setAutoPilot(new PeersClientAutoPilot {
          override def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
            case PeersClient.Request(GetNodeData(_), _, _) =>
              sender ! PeersClient.Response(failingPeer, NodeData(Nil))
              None
          }
        })

        regularSync ! RegularSync.Start

        fishForBlacklistPeer(failingPeer)
      }
      "blacklist peer which returns invalid node" in new MissingStateNodeFixture(testSystem) {
        val failingPeer: Peer = peerByNumber(1)
        peersClient.setAutoPilot(new PeersClientAutoPilot {
          override def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
            case PeersClient.Request(GetNodeData(_), _, _) =>
              sender ! PeersClient.Response(failingPeer, NodeData(List(ByteString("foo"))))
              None
          }
        })

        regularSync ! RegularSync.Start

        fishForBlacklistPeer(failingPeer)
      }
      "retry fetching node if validation failed" in new MissingStateNodeFixture(testSystem) {
        def fishForFailingBlockNodeRequest(): Boolean = peersClient.fishForSpecificMessage() {
          case PeersClient.Request(GetNodeData(hash :: Nil), _, _) if hash == Block.hash(failingBlock) => true
        }

        class WrongNodeDataPeersClientAutoPilot(var handledRequests: Int = 0) extends PeersClientAutoPilot {
          override def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
            case PeersClient.Request(GetNodeData(_), _, _) =>
              val response = handledRequests match {
                case 0 => Some(PeersClient.Response(peerByNumber(1), NodeData(Nil)))
                case 1 => Some(PeersClient.Response(peerByNumber(2), NodeData(List(ByteString("foo")))))
                case _ => None
              }

              response.foreach(sender ! _)
              Some(new WrongNodeDataPeersClientAutoPilot(handledRequests + 1))
          }
        }

        peersClient.setAutoPilot(new WrongNodeDataPeersClientAutoPilot())

        regularSync ! RegularSync.Start

        fishForFailingBlockNodeRequest()
        fishForFailingBlockNodeRequest()
        fishForFailingBlockNodeRequest()
      }
      "save fetched node" in new Fixture(testSystem) {
        implicit val ec: ExecutionContext = system.dispatcher
        override lazy val blockchain: BlockchainImpl = stub[BlockchainImpl]
        override lazy val ledger: TestLedgerImpl = stub[TestLedgerImpl]
        val failingBlock: Block = testBlocksChunked.head.head
        peersClient.setAutoPilot(new PeersClientAutoPilot)

        (ledger.resolveBranch _).when(*).returns(NewBetterBranch(Nil))
        (ledger
          .importBlock(_: Block)(_: ExecutionContext))
          .when(*, *)
          .returns(Future.failed(new MissingNodeException(Block.hash(failingBlock))))

        var saveNodeWasCalled: Boolean = false
        val nodeData = List(ByteString(failingBlock.header.toBytes: Array[Byte]))
        (blockchain.getBestBlockNumber _).when().returns(0)
        (blockchain.getBlockHeaderByNumber _).when(*).returns(Some(genesis.header))
        (blockchain.saveNode _)
          .when(*, *, *)
          .onCall((hash, encoded, totalDifficulty) => {
            val expectedNode = nodeData.head

            hash should be(kec256(expectedNode))
            encoded should be(expectedNode.toArray)
            totalDifficulty should be(Block.number(failingBlock))

            saveNodeWasCalled = true
          })

        regularSync ! RegularSync.Start

        awaitCond(saveNodeWasCalled)
      }
    }

    "catching the top" should {
      "ignore mined blocks" in new Fixture(testSystem) {
        override lazy val ledger: TestLedgerImpl = stub[TestLedgerImpl]

        val minedBlock: Block = testBlocks.head

        regularSync ! RegularSync.Start
        regularSync ! RegularSync.MinedBlock(minedBlock)

        Thread.sleep(remainingOrDefault.toMillis)

        ommersPool.expectMsg(AddOmmers(List(minedBlock.header)))
        (ledger.importBlock(_: Block)(_: ExecutionContext)).verify(*, *).never()
      }
      "ignore new blocks if they are too new" in new Fixture(testSystem) {
        override lazy val ledger: TestLedgerImpl = stub[TestLedgerImpl]

        val newBlock: Block = testBlocks.last

        regularSync ! RegularSync.Start
        peerEventBus.expectMsgClass(classOf[Subscribe])

        peerEventBus.reply(MessageFromPeer(NewBlock(newBlock, 1), defaultPeer.id))

        Thread.sleep(remainingOrDefault.toMillis)

        (ledger.importBlock(_: Block)(_: ExecutionContext)).verify(*, *).never()
      }
    }

    "on top" should {
      abstract class OnTopFixture(system: ActorSystem) extends Fixture(system) {
        val newBlock: Block = getBlock(Block.number(testBlocks.last) + 1, testBlocks.last)

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
              Future.successful(BlockImportedToTop(List(BlockData(newBlock, Nil, Block.number(newBlock)))))
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
          blockFetcher ! MessageFromPeer(NewBlock(block, Block.number(block)), peer.id)

        def goToTop(): Unit = {
          regularSync ! RegularSync.Start

          waitForSubscription()
          sendLastTestBlockAsTop()

          awaitCond(importedLastTestBlock)
        }
      }

      "import received new block" in new OnTopFixture(testSystem) {
        goToTop()

        sendNewBlock()

        awaitCond(importedNewBlock)
      }
      "broadcast imported block" in new OnTopFixture(testSystem) {
        goToTop()

        etcPeerManager.expectMsg(GetHandshakedPeers)
        etcPeerManager.reply(HandshakedPeers(handshakedPeers))

        sendNewBlock()

        etcPeerManager.fishForSpecificMessageMatching() {
          case EtcPeerManagerActor.SendMessage(message, _) =>
            message.underlyingMsg match {
              case NewBlock(block, _) if block == newBlock => true
              case _ => false
            }
          case _ => false
        }
      }
      "update ommers for imported block" in new OnTopFixture(testSystem) {
        goToTop()

        sendNewBlock()

        ommersPool.expectMsg(RemoveOmmers(newBlock.header :: newBlock.body.uncleNodesList.toList))
      }
      "fetch hashes if received NewHashes message" in new OnTopFixture(testSystem) {
        goToTop()

        blockFetcher !
          MessageFromPeer(NewBlockHashes(List(BlockHash(Block.hash(newBlock), Block.number(newBlock)))), defaultPeer.id)

        peersClient.expectMsgPF() {
          case PeersClient.Request(GetBlockHeaders(_, _, _, _), _, _) => true
        }
      }
    }
  }

  abstract class Fixture(_system: ActorSystem)
      extends TestKitBase
      with EphemBlockchainTestSetup
      with TestSyncConfig
      with SecureRandomBuilder {
    implicit override lazy val system: ActorSystem = _system
    override lazy val syncConfig: SyncConfig =
      defaultSyncConfig.copy(blockHeadersPerRequest = 2, blockBodiesPerRequest = 2)
    val handshakedPeers: PeersMap = (0 to 5).map(peerId).map(getPeer).map(peer => (peer, getPeerInfo(peer))).toMap
    val defaultPeer: Peer = peerByNumber(0)

    val etcPeerManager: TestProbe = TestProbe()
    val peerEventBus: TestProbe = TestProbe()
    val ommersPool: TestProbe = TestProbe()
    val pendingTransactionsManager: TestProbe = TestProbe()
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
          system.scheduler
        )
        .withDispatcher("akka.actor.default-dispatcher"))

    // scalastyle:off magic.number
    val defaultHeader = BlockHeader(
      parentHash = bEmpty,
      ommersHash = bEmpty,
      beneficiary = bEmpty,
      stateRoot = bEmpty,
      transactionsRoot = bEmpty,
      receiptsRoot = bEmpty,
      logsBloom = bEmpty,
      difficulty = 1000000,
      number = 1,
      gasLimit = 1000000,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = bEmpty,
      mixHash = bEmpty,
      nonce = bEmpty
    )

    val defaultTx = Transaction(
      nonce = 42,
      gasPrice = 1,
      gasLimit = 90000,
      receivingAddress = Address(123),
      value = 0,
      payload = bEmpty)

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
      (1 to amount).toList.foldLeft[List[Block]](Nil)((generated, number) =>
        generated :+ getBlock(number, generated.lastOption.getOrElse(parent)))

    def getBlock(nr: BigInt, parent: Block): Block = {
      val header = defaultHeader.copy(extraData = randomHash(), number = nr, parentHash = Block.hash(parent))
      val ommer = defaultHeader.copy(extraData = randomHash())
      val tx = defaultTx.copy(payload = randomHash())
      val stx = SignedTransaction.sign(tx, keyPair, None)

      Block(header, BlockBody(List(stx.tx), List(ommer)))
    }

    def peerId(number: Int): PeerId = PeerId(s"peer_$number")

    def getPeer(id: PeerId): Peer =
      Peer(new InetSocketAddress("127.0.0.1", 0), TestProbe(id.value).ref, incomingConnection = false)

    def getPeerInfo(peer: Peer): PeerInfo = {
      val status = Status(1, 1, 1, ByteString(s"${peer.id}_bestHash"), ByteString("unused"))
      PeerInfo(status, forkAccepted = true, totalDifficulty = status.totalDifficulty, maxBlockNumber = 0)
    }

    def peerByNumber(number: Int): Peer = handshakedPeers.keys.toList.sortBy(_.id.value).apply(number)

    def blockHeadersRequest(fromChunk: Int): PeersClient.Request[GetBlockHeaders] = PeersClient.Request.create(
      GetBlockHeaders(
        Left(testBlocksChunked(fromChunk).headNumberUnsafe),
        syncConfig.blockHeadersPerRequest,
        skip = 0,
        reverse = false),
      PeersClient.BestPeer
    )

    def fishForBlacklistPeer(peer: Peer): PeersClient.BlacklistPeer =
      peersClient.fishForSpecificMessage() {
        case msg @ PeersClient.BlacklistPeer(id, _) if id == peer.id => msg
      }

    class TestLedgerImpl extends LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus, system.dispatcher) {
      protected val results = mutable.Map[ByteString, () => Future[BlockImportResult]]()
      protected val importedBlocks = mutable.Set[Block]()

      override def importBlock(block: Block)(
          implicit blockExecutionContext: ExecutionContext): Future[BlockImportResult] = {
        importedBlocks.add(block)
        results(Block.hash(block))()
      }

      def setImportResult(block: Block, result: () => Future[BlockImportResult]): Unit =
        results(block.header.hash) = result

      def didTryToImportBlock(block: Block): Boolean = importedBlocks.exists(Block.hash(_) == Block.hash(block))

      def bestBlock: Block = importedBlocks.maxBy(Block.number)
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
              val nr = Block.number(b)
              minBlock <= nr && nr < maxBlock
            })
            .map(_.header)
            .sortBy(_.number)
          sender ! PeersClient.Response(defaultPeer, BlockHeaders(matchingHeaders))
          None
        case PeersClient.Request(GetBlockBodies(hashes), _, _) =>
          val matchingBodies = hashes.flatMap(hash => blocks.find(b => Block.hash(b) == hash)).map(_.body)
          sender ! PeersClient.Response(defaultPeer, BlockBodies(matchingBodies))
          None
        case PeersClient.Request(GetNodeData(hash :: Nil), _, _) =>
          sender ! PeersClient.Response(
            defaultPeer,
            NodeData(List(ByteString(blocks.byHashUnsafe(hash).header.toBytes: Array[Byte]))))
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
      def headNumberUnsafe: BigInt = Block.number(blocks.head)
      def headNumber: Option[BigInt] = blocks.headOption.map(Block.number)
      def headers: List[BlockHeader] = blocks.map(_.header)
      def hashes: List[ByteString] = headers.map(_.hash)
      def bodies: List[BlockBody] = blocks.map(_.body)
      def numbers: List[BigInt] = blocks.map(Block.number)
      def numberAt(index: Int): Option[BigInt] = blocks.get(index).map(Block.number)
      def numberAtUnsafe(index: Int): BigInt = numberAt(index).get
      def byHash(hash: ByteString): Option[Block] = blocks.find(b => Block.hash(b) == hash)
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

      def fishForSpecificMessageMatching[T](max: FiniteDuration = probe.remainingOrDefault)(
          predicate: Any => Boolean): T =
        probe.fishForSpecificMessage(max) {
          case msg if predicate(msg) => msg.asInstanceOf[T]
        }

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
}
