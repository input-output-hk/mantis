package io.iohk.ethereum
package consensus
package ethash

import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.consensus.ethash.EthashMiner.MinerMsg
import io.iohk.ethereum.consensus.ethash.validators.EthashBlockHeaderValidator
import io.iohk.ethereum.consensus.validators.std.StdBlockHeaderValidator
import io.iohk.ethereum.consensus.validators.{BlockHeaderError, BlockHeaderValid, BlockHeaderValidator}
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.{BlockExecutionError, BlockExecutionSuccess}
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BlockchainConfig

/**
 * Implements standard Ethereum consensus (ethash PoW).
 */
class EthashConsensus(
  blockchainConfig: BlockchainConfig,
  val config: FullConsensusConfig[EthashConfig]
) extends StdConsensus {

  type Config = EthashConfig

  private[this] val defaultValidator = new StdBlockHeaderValidator(blockchainConfig)
  private[this] val powValidator = new EthashBlockHeaderValidator(blockchainConfig)

  private[this] val ethashValidator = new BlockHeaderValidator {
    def validate(
      blockHeader: BlockHeader,
      getBlockHeaderByHash: ByteString ⇒ Option[BlockHeader]
    ): Either[BlockHeaderError, BlockHeaderValid] = {

      for {
        _ ← defaultValidator.validate(blockHeader, getBlockHeaderByHash)
        _ ← powValidator.validate(blockHeader, getBlockHeaderByHash)
      } yield BlockHeaderValid
    }
  }

  private[this] val atomicMiner = new AtomicReference[Option[ActorRef]](None)
  private[this] def sendMiner(msg: MinerMsg): Unit =
    atomicMiner.get().foreach(_ ! msg)

  private[this] def startMiningProcess(node: Node): Unit = {
    atomicMiner.get() match {
      case None ⇒
        val minerBuilder = new EthashMinerBuilder(node, config.specific)
        val miner = minerBuilder.miner
        atomicMiner.set(Some(miner))

        sendMiner(EthashMiner.StartMining)

      case _ ⇒
    }
  }

  private[this] def stopMiningProcess(): Unit = {
    sendMiner(EthashMiner.StopMining)
  }

  def blockHeaderValidator: BlockHeaderValidator = ethashValidator

  /**
   * Starts the consensus protocol on the current `node`.
   */
  def startProtocol(node: Node): Unit = {
    if(config.miningEnabled) {
      startMiningProcess(node)
    }
  }

  def stopProtocol(): Unit = {
    if(config.miningEnabled) {
      stopMiningProcess()
    }
  }

  def protocol: Protocol = Protocol.Ethash

  override def validateBlockBeforeExecution(
    block: Block,
    getBlockHeaderByHash: GetBlockHeaderByHash,
    getNBlocksBack: GetNBlocksBack
  ): Either[BlockExecutionError.ValidationBeforeExecError, BlockExecutionSuccess] = {

    val blockHeaderV = validators.blockHeaderValidator
    val blockV = validators.blockValidator
    val ommersV = validators.ommersValidator

    val header = block.header
    val body = block.body

    val result = for {
      _ <- blockHeaderV.validate(header, getBlockHeaderByHash)
      _ <- blockV.validateHeaderAndBody(header, body)
      _ <- ommersV.validate(header.parentHash, header.number, body.uncleNodesList,
        getBlockHeaderByHash, getNBlocksBack)
    } yield BlockExecutionSuccess

    result.left.map(ValidationBeforeExecError)
  }
}
