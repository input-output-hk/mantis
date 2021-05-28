package io.iohk.ethereum.consensus.pow.miners

import akka.actor.{ActorRef => ClassicActorRef}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator
import io.iohk.ethereum.consensus.pow.PoWMiningCoordinator.CoordinatorProtocol
import io.iohk.ethereum.consensus.pow.miners.MinerProtocol.{MiningResult, MiningSuccessful}
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.jsonrpc.EthMiningService
import io.iohk.ethereum.jsonrpc.EthMiningService.SubmitHashRateRequest
import io.iohk.ethereum.utils.{ByteStringUtils, Logger}
import monix.execution.CancelableFuture

trait Miner extends Logger {
  def processMining(bestBlock: Block): CancelableFuture[CoordinatorProtocol]

  def handleMiningResult(
      miningResult: MiningResult,
      syncController: ClassicActorRef,
      block: Block
  ): CoordinatorProtocol = {
    miningResult match {
      case MiningSuccessful(_, mixHash, nonce) =>
        log.info(
          "Mining successful with {} and nonce {}",
          ByteStringUtils.hash2string(mixHash),
          ByteStringUtils.hash2string(nonce)
        )

        syncController ! SyncProtocol.MinedBlock(
          block.copy(header = block.header.copy(nonce = nonce, mixHash = mixHash))
        )
        PoWMiningCoordinator.MiningSuccessful
      case _ =>
        log.info("Mining unsuccessful")
        PoWMiningCoordinator.MiningUnsuccessful
    }
  }

  def submitHashRate(ethMiningService: EthMiningService, time: Long, mineResult: MiningResult): Unit = {
    val hashRate = if (time > 0) (mineResult.triedHashes.toLong * 1000000000) / time else Long.MaxValue
    ethMiningService.submitHashRate(SubmitHashRateRequest(hashRate, ByteString("mantis-miner")))
  }
}
