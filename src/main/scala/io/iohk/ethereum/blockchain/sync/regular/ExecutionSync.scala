package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{Actor, ActorLogging, ActorRef, AllForOneStrategy, Props, Scheduler, SupervisorStrategy}
import io.iohk.ethereum.blockchain.sync.{Blacklist, SyncProtocol}
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.{NewCheckpoint, ProgressState}
import io.iohk.ethereum.consensus.validators.BlockValidator
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain.{Block, Blockchain, BlockchainReader}
import io.iohk.ethereum.ledger.{BlockImport, BranchResolution}
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}

class ExecutionSync(
    fetcherService: FetcherService,
    blockImporterService: BlockImporterService,
    peersClient: ActorRef,
    etcPeerManager: ActorRef,
    peerEventBus: ActorRef,
    blockImport: BlockImport,
    blockchain: Blockchain,
    blockchainReader: BlockchainReader,
    stateStorage: StateStorage,
    branchResolution: BranchResolution,
    blockValidator: BlockValidator,
    blacklist: Blacklist,
    syncConfig: SyncConfig,
    ommersPool: ActorRef,
    pendingTransactionsManager: ActorRef,
    scheduler: Scheduler
) extends Actor
    with ActorLogging {

  val broadcaster: ActorRef = context.actorOf(
    BlockBroadcasterActor
      .props(new BlockBroadcast(etcPeerManager), peerEventBus, etcPeerManager, blacklist, syncConfig, scheduler),
    "block-broadcaster"
  )

  override def receive: Receive = running(
    ProgressState(initialBlock = 0, currentBlock = 0, bestKnownNetworkBlock = 0)
  )

  def running(progressState: ProgressState): Receive = {

    case SyncProtocol.Start =>
      log.info("Starting regular sync")
      peerEventBus ! Subscribe(
        MessageClassifier(
          Set(Codes.NewBlockCode, Codes.NewBlockHashesCode, Codes.BlockHeadersCode),
          PeerSelector.AllPeers
        )
      )
      val listOfPeersToFetchFromWithBestBlock: List[(Peer, Int)] = ???
      val startFrom: BigInt = blockchainReader.getBestBlockNumber()
      listOfPeersToFetchFromWithBestBlock.map(peerWithBlockNumber =>
        fetcherService.fetchBlocksUntil(peerWithBlockNumber._1, Left(startFrom), Left(peerWithBlockNumber._2))
      )
    //pass peers containing fetchedBlocks streams to the ChainManager?
    //TODO: how to run it in the loop?

    case SyncProtocol.MinedBlock(block) =>
      log.info(s"Block mined [number = {}, hash = {}]", block.number, block.header.hashAsHexString)
    //pass mined block to the ChainManager?

    case NewCheckpoint(block) =>
      log.info(s"Received new checkpoint for block ${ByteStringUtils.hash2string(block.header.parentHash)}")
    //pass checkpoint block to the ChainManager?

    case SyncProtocol.GetStatus =>
      sender() ! progressState.toStatus

    //TODO: add handling of messages from peerEventBus and passing it onto ChainManager
//      log.info(s"Got information about new block [number = $blockNumber]")
//      val newState = progressState.copy(bestKnownNetworkBlock = blockNumber)
//      context.become(running(newState))

    //TODO: replace with a message  from ChainManagerService
//    case ProgressProtocol.ImportedBlock(blockNumber, internally) =>
//      log.info(s"Imported new block [number = $blockNumber, internally = $internally]")
//      val newState = progressState.copy(currentBlock = blockNumber)
//      context.become(running(newState))
  }

  override def supervisorStrategy: SupervisorStrategy = AllForOneStrategy()(SupervisorStrategy.defaultDecider)

  override def postStop(): Unit =
    log.info("Regular Sync stopped")
}

object ExecutionSync {
  // scalastyle:off parameter.number
  def props(
      fetcherService: FetcherService,
      blockImporterService: BlockImporterService,
      peersClient: ActorRef,
      etcPeerManager: ActorRef,
      peerEventBus: ActorRef,
      blockImport: BlockImport,
      blockchain: Blockchain,
      blockchainReader: BlockchainReader,
      stateStorage: StateStorage,
      branchResolution: BranchResolution,
      blockValidator: BlockValidator,
      blacklist: Blacklist,
      syncConfig: SyncConfig,
      ommersPool: ActorRef,
      pendingTransactionsManager: ActorRef,
      scheduler: Scheduler
  ): Props =
    Props(
      new ExecutionSync(
        fetcherService,
        blockImporterService,
        peersClient,
        etcPeerManager,
        peerEventBus,
        blockImport,
        blockchain,
        blockchainReader,
        stateStorage,
        branchResolution,
        blockValidator,
        blacklist,
        syncConfig,
        ommersPool,
        pendingTransactionsManager,
        scheduler
      )
    )

  case class NewCheckpoint(block: Block)

  case class ProgressState(
      initialBlock: BigInt,
      currentBlock: BigInt,
      bestKnownNetworkBlock: BigInt
  )
}
