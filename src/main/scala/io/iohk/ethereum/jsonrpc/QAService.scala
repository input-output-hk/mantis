package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.ByteString
import cats.implicits._
import enumeratum._
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.MockedMinerResponses._
import io.iohk.ethereum.consensus.pow.miners.MockedMiner.{MineBlocks, MockedMinerResponse, MockedMinerResponses}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Block, Blockchain, Checkpoint}
import io.iohk.ethereum.jsonrpc.QAService.MineBlocksResponse.MinerResponseType
import io.iohk.ethereum.jsonrpc.QAService._
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import monix.eval.Task
import mouse.all._

class QAService(
    consensus: Consensus,
    blockchain: Blockchain,
    checkpointBlockGenerator: CheckpointBlockGenerator,
    blockchainConfig: BlockchainConfig,
    syncController: ActorRef
) extends Logger {

  /**
    * qa_mineBlocks that instructs mocked miner to mine given number of blocks
    *
    * @param req with requested block's data
    * @return nothing
    */
  def mineBlocks(req: MineBlocksRequest): ServiceResponse[MineBlocksResponse] = {
    consensus
      .askMiner(MineBlocks(req.numBlocks, req.withTransactions, req.parentBlock))
      .map(_ |> (MineBlocksResponse(_)) |> (_.asRight))
      .onErrorHandle { throwable =>
        log.warn("Unable to mine requested blocks", throwable)
        Left(JsonRpcError.InternalError)
      }
  }

  def generateCheckpoint(
      req: GenerateCheckpointRequest
  ): ServiceResponse[GenerateCheckpointResponse] = {
    val hash = req.blockHash.orElse(blockchain.getBestBlock().map(_.hash))
    hash match {
      case Some(hashValue) =>
        Task {
          val parent =
            blockchain.getBlockByHash(hashValue).orElse(blockchain.getBestBlock()).getOrElse(blockchain.genesisBlock)
          val checkpoint = generateCheckpoint(hashValue, req.privateKeys)
          val checkpointBlock: Block = checkpointBlockGenerator.generate(parent, checkpoint)
          syncController ! NewCheckpoint(checkpointBlock)
          Right(GenerateCheckpointResponse(checkpoint))
        }
      case None => Task.now(Left(JsonRpcError.BlockNotFound))
    }
  }

  private def generateCheckpoint(blockHash: ByteString, privateKeys: Seq[ByteString]): Checkpoint = {
    val keys = privateKeys.map { key =>
      crypto.keyPairFromPrvKey(key.toArray)
    }
    val signatures = keys.map(ECDSASignature.sign(blockHash.toArray, _, None))
    Checkpoint(signatures)
  }

  def getFederationMembersInfo(
      req: GetFederationMembersInfoRequest
  ): ServiceResponse[GetFederationMembersInfoResponse] = {
    Task {
      Right(GetFederationMembersInfoResponse(blockchainConfig.checkpointPubKeys.toList))
    }
  }
}

object QAService {
  case class MineBlocksRequest(numBlocks: Int, withTransactions: Boolean, parentBlock: Option[ByteString] = None)
  case class MineBlocksResponse(responseType: MinerResponseType, message: Option[String])
  object MineBlocksResponse {
    def apply(minerResponse: MockedMinerResponse): MineBlocksResponse =
      MineBlocksResponse(MinerResponseType(minerResponse), extractMessage(minerResponse))

    private def extractMessage(response: MockedMinerResponse): Option[String] = response match {
      case MinerIsWorking | MiningOrdered | MinerNotExist => None
      case MiningError(msg) => Some(msg)
      case MinerNotSupported(msg) => Some(msg.toString)
    }

    sealed trait MinerResponseType extends EnumEntry
    object MinerResponseType extends Enum[MinerResponseType] {
      val values = findValues

      case object MinerIsWorking extends MinerResponseType
      case object MiningOrdered extends MinerResponseType
      case object MinerNotExist extends MinerResponseType
      case object MiningError extends MinerResponseType
      case object MinerNotSupport extends MinerResponseType

      def apply(minerResponse: MockedMinerResponse): MinerResponseType = minerResponse match {
        case MockedMinerResponses.MinerIsWorking => MinerIsWorking
        case MockedMinerResponses.MiningOrdered => MiningOrdered
        case MockedMinerResponses.MinerNotExist => MinerNotExist
        case MockedMinerResponses.MiningError(_) => MiningError
        case MockedMinerResponses.MinerNotSupported(_) => MinerNotSupport
      }
    }
  }

  case class GenerateCheckpointRequest(privateKeys: Seq[ByteString], blockHash: Option[ByteString])
  case class GenerateCheckpointResponse(checkpoint: Checkpoint)

  case class GetFederationMembersInfoRequest()
  case class GetFederationMembersInfoResponse(membersPublicKeys: Seq[ByteString])
}
