package io.iohk.ethereum.blockchain.sync.regular

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.MinedBlock
import io.iohk.ethereum.blockchain.sync.{BlockBroadcast, EphemBlockchainTestSetup, TestSyncConfig}
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.network.EtcPeerManagerActor.{HandshakedPeers, PeerInfo}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.Subscribe
import io.iohk.ethereum.network.p2p.messages.CommonMessages.{NewBlock, Status}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.NodeData
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.ommers.OmmersPool.{AddOmmers, RemoveOmmers}
import io.iohk.ethereum.transactions.PendingTransactionsManager.{AddUncheckedTransactions, RemoveTransactions}
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

// scalastyle:off magic.number
class OldRegularSyncSpec extends WordSpec with Matchers with MockFactory with Eventually {

  "Old RegularSync" when {

    "receiving NewBlock message" should {

      "handle import to the main chain" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()
        val blockData = BlockData(block, Seq.empty, defaultTd)
        (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(block, *).returning(futureResult(BlockImportedToTop(List(blockData))))
        (broadcaster.broadcastBlock _).expects(NewBlock(block, defaultTd), handshakedPeers)

        sendBlockHeaders(Seq.empty)

        sendNewBlockMsg(block)

        ommersPool.expectMsg(RemoveOmmers(block.header :: block.body.uncleNodesList.toList))
        txPool.expectMsg(RemoveTransactions(block.body.transactionList.toList))
        system.terminate()
      }

      "handle chain reorganisation" in new TestSetup {
        startSyncing()
        val newBlock: Block = getBlock()
        val oldBlock: Block = getBlock()

        (ledger.importBlock(_: Block)(_: ExecutionContext))
          .expects(newBlock, *)
          .returning(futureResult(ChainReorganised(List(oldBlock), List(newBlock), List(defaultTd))))
        (broadcaster.broadcastBlock _).expects(NewBlock(newBlock, defaultTd), handshakedPeers)

        sendBlockHeaders(Seq.empty)
        sendNewBlockMsg(newBlock)

        ommersPool.expectMsg(AddOmmers(List(oldBlock.header)))
        txPool.expectMsg(AddUncheckedTransactions(oldBlock.body.transactionList))

        ommersPool.expectMsg(RemoveOmmers(newBlock.header :: newBlock.body.uncleNodesList.toList))
        txPool.expectMsg(RemoveTransactions(newBlock.body.transactionList.toList))
        system.terminate()
      }

      "handle duplicate" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()

        (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(block, *).returning(futureResult(DuplicateBlock))
        (broadcaster.broadcastBlock _).expects(*, *).never()

        sendBlockHeaders(Seq.empty)
        sendNewBlockMsg(block)

        ommersPool.expectNoMessage(1.second)
        txPool.expectNoMessage()
        system.terminate()
      }

      "handle unknown parent" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()

        (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(block, *).returning(futureResult(UnknownParent))
        (broadcaster.broadcastBlock _).expects(*, *).never()

        sendBlockHeaders(Seq.empty)
        sendNewBlockMsg(block)

        ommersPool.expectNoMessage(1.second)
        txPool.expectNoMessage()
        system.terminate()
      }

      "handle enqueuing" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()

        (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(block, *).returning(futureResult(BlockEnqueued))
        (broadcaster.broadcastBlock _).expects(*, *).never()

        sendBlockHeaders(Seq.empty)
        sendNewBlockMsg(block)

        ommersPool.expectMsg(AddOmmers(List(block.header)))
        txPool.expectNoMessage()
        system.terminate()
      }

      "handle block error" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()

        (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(block, *).returning(futureResult(BlockImportFailed("error")))
        (broadcaster.broadcastBlock _).expects(*, *).never()

        sendBlockHeaders(Seq.empty)
        sendNewBlockMsg(block)

        ommersPool.expectNoMessage(1.second)
        txPool.expectNoMessage()

        regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe true
        system.terminate()
      }

      "handle missing state nodes" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()

        val blockData = BlockData(block, Seq.empty, 0)
        inSequence {
          (ledger.importBlock(_: Block)(_: ExecutionContext))
            .expects(block, *)
            .returning(Future.failed(new MissingNodeException(missingNodeHash)))

        }

        sendBlockHeaders(Seq.empty)
        sendNewBlockMsg(block)

        regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe false

        system.terminate()
      }
    }

    "receiving NewBlockHashes message" should {

      "handle newBlockHash message" in new TestSetup {
        startSyncing()
        val blockHash: BlockHash = randomBlockHash()

        (ledger.checkBlockStatus _).expects(blockHash.hash).returning(UnknownBlock)
        sendBlockHeaders(Seq.empty)
        sendNewBlockHashMsg(Seq(blockHash))

        val headers = GetBlockHeaders(Right(blockHash.hash), 1, 0, reverse = false)
        etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(headers, peer1Id))
        system.terminate()
      }

      "filter out hashes that are already in chain or queue" in new TestSetup {
        startSyncing()

        val blockHashes: IndexedSeq[BlockHash] = (1 to 4).map(num => randomBlockHash(num))

        blockHashes.foreach{ blockHash =>
          if (blockHash.number == 1)
            (ledger.checkBlockStatus _).expects(blockHash.hash).returning(InChain)
          else if (blockHash.number == 2)
            (ledger.checkBlockStatus _).expects(blockHash.hash).returning(Queued)
          else
            (ledger.checkBlockStatus _).expects(blockHash.hash).returning(UnknownBlock)
        }

        sendBlockHeaders(Seq.empty)
        sendNewBlockHashMsg(blockHashes)

        val hashesRequested: IndexedSeq[BlockHash] = blockHashes.takeRight(2)
        val headers = GetBlockHeaders(Right(hashesRequested.head.hash), hashesRequested.length, 0, reverse = false)

        etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(headers, peer1Id))

        system.terminate()
      }

      "blacklist peer sending ancient block hashes" in new TestSetup {
        startSyncing()
        val blockHash: BlockHash = randomBlockHash()
        storagesInstance.storages.appStateStorage.putBestBlockNumber(blockHash.number + syncConfig.maxNewBlockHashAge + 1)
        sendBlockHeaders(Seq.empty)
        sendNewBlockHashMsg(Seq(blockHash))

        regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe true
        system.terminate()
      }

      "handle at most 64 new hashes in one request" in new ShortResponseTimeout with TestSetup {
        startSyncing()
        import syncConfig.maxNewHashes

        val blockHashes: IndexedSeq[BlockHash]= (1 to maxNewHashes + 1).map(num => randomBlockHash(num))
        val headers = GetBlockHeaders(Right(blockHashes.head.hash), maxNewHashes, 0, reverse = false)

        blockHashes.take(maxNewHashes).foreach{ blockHash =>
          (ledger.checkBlockStatus _).expects(blockHash.hash).returning(UnknownBlock)
        }

        sendBlockHeaders(Seq.empty)
        sendNewBlockHashMsg(blockHashes)

        etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(headers, peer1Id))

        system.terminate()
      }
    }

    "receiving MinedBlock message" should {

      "handle import to the main chain" in new TestSetup {
        startSyncing()

        val block: Block = getBlock()
        val blockData = BlockData(block, Seq.empty, defaultTd)
        (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(block, *).returning(futureResult(BlockImportedToTop(List(blockData))))
        (broadcaster.broadcastBlock _).expects(NewBlock(block, defaultTd), handshakedPeers)

        sendMinedBlockMsg(block)

        txPool.expectMsg(RemoveTransactions(block.body.transactionList.toList))
        system.terminate()
      }

      "handle chain reorganisation" in new TestSetup {
        startSyncing()
        val newBlock: Block = getBlock()
        val oldBlock: Block = getBlock()

        (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(newBlock, *)
          .returning(futureResult(ChainReorganised(List(oldBlock), List(newBlock), List(defaultTd))))
        (broadcaster.broadcastBlock _).expects(NewBlock(newBlock, defaultTd), handshakedPeers)

        sendMinedBlockMsg(newBlock)

        ommersPool.expectMsg(AddOmmers(List(oldBlock.header)))
        txPool.expectMsg(AddUncheckedTransactions(oldBlock.body.transactionList))

        ommersPool.expectMsg(RemoveOmmers(newBlock.header :: newBlock.body.uncleNodesList.toList))
        txPool.expectMsg(RemoveTransactions(newBlock.body.transactionList.toList))
        system.terminate()
      }

      "handle duplicate" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()

        (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(block, *).returning(futureResult(DuplicateBlock))
        (broadcaster.broadcastBlock _).expects(*, *).never()

        sendMinedBlockMsg(block)

        ommersPool.expectNoMessage(1.second)
        txPool.expectNoMessage()
        system.terminate()
      }

      "handle enqueuing" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()

        (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(block, *).returning(futureResult(BlockEnqueued))
        (broadcaster.broadcastBlock _).expects(*, *).never()

        sendMinedBlockMsg(block)

        ommersPool.expectMsg(AddOmmers(List(block.header)))
        txPool.expectNoMessage()
        system.terminate()
      }

      "handle block error" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()

        (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(block, *).returning(futureResult(BlockImportFailed("error")))
        (broadcaster.broadcastBlock _).expects(*, *).never()

        sendMinedBlockMsg(block)

        ommersPool.expectNoMessage(1.second)
        txPool.expectNoMessage()
        system.terminate()
      }
    }

    "receiving BlockHeaders message" should {

      "handle a new better branch" in new TestSetup {
        startSyncing()
        val oldBlocks: IndexedSeq[Block] = (1 to 2).map(_ => getBlock())
        val newBlocks: IndexedSeq[Block] = (1 to 2).map(_ => getBlock())

        (ledger.resolveBranch _).expects(newBlocks.map(_.header)).returning(NewBetterBranch(oldBlocks))

        sendBlockHeadersFromBlocks(newBlocks)

        etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(GetBlockBodies(newBlocks.map(_.header.hash)), peer1Id))
        txPool.expectMsg(AddUncheckedTransactions(oldBlocks.flatMap(_.body.transactionList)))
        ommersPool.expectMsg(AddOmmers(oldBlocks.head.header))
        system.terminate()
      }

      "handle no branch change" in new TestSetup {
        startSyncing()
        val newBlocks: IndexedSeq[Block] = (1 to 2).map(_ => getBlock())

        (ledger.resolveBranch _).expects(newBlocks.map(_.header)).returning(NoChainSwitch)

        sendBlockHeadersFromBlocks(newBlocks)

        ommersPool.expectMsg(AddOmmers(newBlocks.head.header))
        system.terminate()
      }

      // TODO: the following 3 tests are very poor, but fixing that requires re-designing much of the sync actors, with testing in mind
      "handle unknown branch about to be resolved" in new TestSetup {
        startSyncing()
        val newBlocks: IndexedSeq[Block] = (1 to 10).map(_ => getBlock())

        (ledger.resolveBranch _).expects(newBlocks.map(_.header)).returning(UnknownBranch)

        sendBlockHeadersFromBlocks(newBlocks)
        regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe false
        system.terminate()
      }

      "handle unknown branch that can't be resolved" in new TestSetup {
        startSyncing()
        val additionalHeaders: IndexedSeq[BlockHeader] = (1 to 2).map(_ => getBlock().header)
        val newHeaders: IndexedSeq[BlockHeader] =
          getBlock().header.copy(parentHash = additionalHeaders.head.hash) +: (1 to 9).map(_ => getBlock().header)

        inSequence{
          (ledger.resolveBranch _).expects(newHeaders).returning(UnknownBranch)
          (ledger.resolveBranch _).expects(additionalHeaders.reverse ++ newHeaders).returning(UnknownBranch)
        }

        sendBlockHeaders(newHeaders)
        sendBlockHeaders(additionalHeaders)

        eventually(timeout(Span(2, Seconds))){
          regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe true
        }

        ommersPool.expectNoMessage()
        system.terminate()
      }

      "return to normal syncing mode after successful branch resolution" in new TestSetup {
        startSyncing()
        val additionalHeaders: IndexedSeq[BlockHeader] = (1 to 2).map(_ => getBlock().header)
        val newHeaders: IndexedSeq[BlockHeader] =
          getBlock().header.copy(parentHash = additionalHeaders.head.hash) +: (1 to 9).map(_ => getBlock().header)

        inSequence{
          (ledger.resolveBranch _).expects(newHeaders).returning(UnknownBranch)
          (ledger.resolveBranch _).expects(additionalHeaders.reverse ++ newHeaders).returning(NoChainSwitch)
        }

        sendBlockHeaders(newHeaders)
        sendBlockHeaders(additionalHeaders)
        eventually{
          regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe false
        }

        ommersPool.expectMsgClass(classOf[AddOmmers])
        system.terminate()
      }

      "return to normal syncing mode after branch resolution request failed" in new ShortResponseTimeout with TestSetup {
        startSyncing()
        val newHeaders: IndexedSeq[BlockHeader] = (1 to 10).map(_ => getBlock().header)

        (ledger.resolveBranch _).expects(newHeaders).returning(UnknownBranch)

        sendBlockHeaders(newHeaders)
        eventually(timeout(Span(2, Seconds))){
          regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe true
        }

        system.terminate()
      }

      "handle invalid branch" in new TestSetup {
        startSyncing()
        val newBlocks: IndexedSeq[Block] = (1 to 2).map(_ => getBlock())

        (ledger.resolveBranch _).expects(newBlocks.map(_.header)).returning(InvalidBranch)

        sendBlockHeadersFromBlocks(newBlocks)

        eventually(timeout(Span(5, Seconds))){
          regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe true
        }
        system.terminate()
      }
    }

    "receiving BlockBodies message" should {

      "handle missing state nodes" in new TestSetup {
        startSyncing()
        val newBlock: Block = getBlock()
        val blockData = BlockData(newBlock, Seq.empty, 0)

        inSequence {
          (ledger.resolveBranch _).expects(Seq(newBlock.header)).returning(NewBetterBranch(Nil))
          (ledger.importBlock(_: Block)(_: ExecutionContext))
            .expects(newBlock, *)
            .returning(Future.failed(new MissingNodeException(missingNodeHash)))
          (ledger.importBlock(_: Block)(_: ExecutionContext))
            .expects(newBlock, *)
            .returning(futureResult(BlockImportedToTop(List(blockData))))
        }

        sendBlockHeadersFromBlocks(Seq(newBlock))
        sendBlockBodiesFromBlocks(Seq(newBlock))
        sendNodeData(Seq(missingNodeValue))

        regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe false

        system.terminate()
      }

      "handle imported blocks by block bodies with same number of block headers and bodies" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()
        val blockData = BlockData(block, Seq.empty, defaultTd)

        (ledger.resolveBranch _).expects(Seq(block.header)).returning(NoChainSwitch).noMoreThanOnce()
        (ledger.importBlock(_: Block)(_: ExecutionContext))
          .expects(block, *)
          .returning(futureResult(BlockImportedToTop(List(blockData))))
          .noMoreThanOnce()

        sendBlockHeadersFromBlocks(Seq(block))
        sendBlockBodiesFromBlocks(Seq(block))

        ommersPool.expectMsg(AddOmmers(block.header))
        regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe false

        // Creating child actor because headersQueue is empty
        etcPeerManager.expectMsgClass(classOf[EtcPeerManagerActor.SendMessage])
        peerEventBus.expectMsgClass(classOf[Subscribe])
        peerEventBus.expectMsgClass(classOf[Subscribe])

        system.terminate()
      }

      "handle imported blocks by block bodies with more block header than bodies" in new TestSetup {
        startSyncing()
        val block1: Block = getBlock()
        val block2: Block = getBlock()
        val blockData = BlockData(block1, Seq.empty, defaultTd)

        (ledger.resolveBranch _).expects(Seq(block1.header, block2.header)).returning(NoChainSwitch).noMoreThanOnce()
        (ledger.importBlock(_: Block)(_: ExecutionContext))
          .expects(block1, *)
          .returning(futureResult(BlockImportedToTop(List(blockData))))
          .noMoreThanOnce()

        sendBlockHeadersFromBlocks(Seq(block1, block2))
        sendBlockBodiesFromBlocks(Seq(block1))

        ommersPool.expectMsg(AddOmmers(block1.header))
        regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe false

        val requestMsg = GetBlockBodies(Seq(block2.header.hash))

        // Creating child actor because requesting more block bodies
        etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(requestMsg, peer1Id))

        system.terminate()
      }

      "not import blocks when headerQueue is empty" in new TestSetup {
        startSyncing()
        val block: Block = getBlock()
        val blockData = BlockData(block, Seq.empty, defaultTd)

        (ledger.importBlock(_: Block)(_: ExecutionContext))
          .expects(block, *)
          .returning(futureResult(BlockImportedToTop(List(blockData))))
          .noMoreThanOnce()

        sendBlockBodiesFromBlocks(Seq(block))

        regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe false

        system.terminate()
      }

      "resume with different peer" when {
        "block bodies are empty" in new TestSetup {
          startSyncing()

          sendBlockBodiesFromBlocks(Seq.empty)
          regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe true

          system.terminate()
        }

        "block import failed" in new TestSetup {
          startSyncing()
          val newBlock: Block = getBlock()

          inSequence {
            (ledger.resolveBranch _).expects(Seq(newBlock.header)).returning(NewBetterBranch(Nil))
            (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(newBlock, *).returning(futureResult(BlockImportFailed("something wrong")))
          }

          sendBlockHeadersFromBlocks(Seq(newBlock))
          sendBlockBodiesFromBlocks(Seq(newBlock))

          regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe true

          system.terminate()
        }

        "got unknown parent" in new TestSetup {
          startSyncing()
          val newBlock: Block = getBlock()

          inSequence {
            (ledger.resolveBranch _).expects(Seq(newBlock.header)).returning(NewBetterBranch(Nil))
            (ledger.importBlock(_: Block)(_: ExecutionContext)).expects(newBlock, *).returning(futureResult(UnknownParent))
          }

          sendBlockHeadersFromBlocks(Seq(newBlock))
          sendBlockBodiesFromBlocks(Seq(newBlock))

          regularSync.underlyingActor.isBlacklisted(peer1Id) shouldBe true

          system.terminate()
        }
      }
    }
  }

  trait TestSetup extends EphemBlockchainTestSetup with SecureRandomBuilder with TestSyncConfig {
    override implicit lazy val system: ActorSystem = ActorSystem("RegularSyncSpec_System")

    implicit val ec: ExecutionContext = system.dispatcher

    def futureResult[A](a: A): Future[A] = {
      Future.successful(a)
    }

    storagesInstance.storages.appStateStorage.putBestBlockNumber(0)

    val etcPeerManager = TestProbe()
    val peerEventBus = TestProbe()
    val ommersPool = TestProbe()
    val txPool = TestProbe()
    val broadcaster: BlockBroadcast = mock[BlockBroadcast]
    override lazy val ledger: Ledger = mock[Ledger]

    val regularSync: TestActorRef[OldRegularSync] = TestActorRef[OldRegularSync](OldRegularSync.props(
      appStateStorage = storagesInstance.storages.appStateStorage,
      etcPeerManager = etcPeerManager.ref,
      peerEventBus = peerEventBus.ref,
      ommersPool = ommersPool.ref,
      pendingTransactionsManager = txPool.ref,
      broadcaster = broadcaster,
      ledger = ledger,
      blockchain = blockchain,
      syncConfig = syncConfig,
      scheduler = system.scheduler
    ))

    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 0), TestProbe().ref, incomingConnection = false)
    val peer1Status = Status(1, 1, 1, ByteString("peer1_bestHash"), ByteString("unused"))
    val peer1Info = PeerInfo(peer1Status, forkAccepted = true, totalDifficulty = peer1Status.totalDifficulty, maxBlockNumber = 0, bestBlockHash = peer1Status.bestHash)
    val peer1Id: PeerId = peer1.id

    val handshakedPeers = Map(peer1 -> peer1Info)

    val missingNodeValue = ByteString("42")
    val missingNodeHash: ByteString = kec256(missingNodeValue)

    def startSyncing(): Unit = {
      regularSync ! OldRegularSync.StartIdle

      regularSync ! HandshakedPeers(handshakedPeers)
      regularSync.underlyingActor.handshakedPeers shouldBe handshakedPeers

      // from RegularSync scheduler
      peerEventBus.expectMsgClass(classOf[Subscribe])

      // from PeerListSupport scheduler
      etcPeerManager.expectMsg(EtcPeerManagerActor.GetHandshakedPeers)
      etcPeerManager.reply(HandshakedPeers(handshakedPeers))

    }

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
      payload = bEmpty
    )

    val defaultTd = 12345

    val keyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)

    def randomHash(): ByteString =
      ObjectGenerators.byteStringOfLengthNGen(32).sample.get

    def getBlock(): Block = {
      val header = defaultHeader.copy(extraData = randomHash())
      val ommer = defaultHeader.copy(extraData = randomHash())
      val tx = defaultTx.copy(payload = randomHash())
      val stx = SignedTransaction.sign(tx, keyPair, None)

      Block(header, BlockBody(List(stx.tx), List(ommer)))
    }

    def sendNewBlockMsg(block: Block): Unit = {
      regularSync ! MessageFromPeer(NewBlock(block, 0), peer1Id)
    }

    def randomBlockHash(blockNum: BigInt = 1): BlockHash = BlockHash(randomHash(), blockNum)

    def sendNewBlockHashMsg(blockHashes: Seq[BlockHash]): Unit = {
      regularSync ! MessageFromPeer(NewBlockHashes(blockHashes), peer1Id)
    }

    def sendMinedBlockMsg(block: Block): Unit = regularSync ! MinedBlock(block)

    def sendBlockHeadersFromBlocks(blocks: Seq[Block]): Unit = sendBlockHeaders(blocks.map(_.header))

    def sendBlockBodiesFromBlocks(blocks: Seq[Block]): Unit = sendBlockBodies(blocks.map(_.body))


    def sendBlockHeaders(headers: Seq[BlockHeader]): Unit = {
      regularSync ! ResponseReceived(peer1, BlockHeaders(headers), 0)
    }

    def sendBlockBodies(bodies: Seq[BlockBody]): Unit = {
      regularSync ! ResponseReceived(peer1, BlockBodies(bodies), 0)
    }

    def sendNodeData(nodeValues: Seq[ByteString]): Unit = {
      regularSync ! ResponseReceived(peer1, NodeData(nodeValues), 0)
    }
  }

  trait ShortResponseTimeout extends TestSyncConfig {
    override lazy val syncConfig: SyncConfig = defaultSyncConfig.copy(peerResponseTimeout = 1.milli)
  }

}
