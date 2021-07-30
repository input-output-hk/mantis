package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.AllForOneStrategy
import akka.actor.Props
import akka.actor.Scheduler
import akka.actor.SupervisorStrategy

import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.Subscribe
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.p2p.messages.Codes

class ExecutionSync(
    fetcherService: FetcherService,
    peersClient: ActorRef,
    peerEventBus: ActorRef,
    blockchainReader: BlockchainReader,
    scheduler: Scheduler
) extends Actor
    with ActorLogging {

  override def receive: Receive = {

    case SyncProtocol.Start =>
      log.info("Starting regular sync")
      peerEventBus ! Subscribe(
        MessageClassifier(
          Set(Codes.NewBlockCode, Codes.NewBlockHashesCode, Codes.BlockHeadersCode),
          PeerSelector.AllPeers
        )
      )
      val listOfPeersToFetchFromWithBestBlock: List[(Peer, Int)] = ??? //peerClient.get
      val startFrom: BigInt = blockchainReader.getBestBlockNumber()
      listOfPeersToFetchFromWithBestBlock.map { case (peerId, peerBestBlockNumber) =>
        fetcherService.fetchBlocksUntil(peerId, Left(startFrom), Left(peerBestBlockNumber))
      }

    //pass peers containing fetchedBlocks streams to the ChainManager
    //TODO: how to run it in the loop?

    case SyncProtocol.MinedBlock(block) =>
      log.info(s"Block mined [number = {}, hash = {}]", block.number, block.header.hashAsHexString)
    //pass mined block to the ChainManager?

    //TODO: add handling of messages from peerEventBus and passing it onto ChainManager

    //TODO: handle messages from ChainManagerService
  }

  override def supervisorStrategy: SupervisorStrategy = AllForOneStrategy()(SupervisorStrategy.defaultDecider)

  override def postStop(): Unit =
    log.info("Execution Sync stopped")
}

object ExecutionSync {
  // scalastyle:off parameter.number
  def props(
      fetcherService: FetcherService,
      peersClient: ActorRef,
      peerEventBus: ActorRef,
      blockchainReader: BlockchainReader,
      scheduler: Scheduler
  ): Props =
    Props(
      new ExecutionSync(
        fetcherService,
        peersClient,
        peerEventBus,
        blockchainReader,
        scheduler
      )
    )
}
