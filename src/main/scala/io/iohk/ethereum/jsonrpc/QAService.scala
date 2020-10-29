package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.ByteString
import cats.implicits._
import enumeratum._
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.consensus.ethash.MinerResponses._
import io.iohk.ethereum.consensus.ethash.MockedMinerProtocol.MineBlocks
import io.iohk.ethereum.consensus.ethash.{MinerResponse, MinerResponses}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Blockchain, Checkpoint}
import io.iohk.ethereum.jsonrpc.QAService.MineBlocksResponse.MinerResponseType
import io.iohk.ethereum.jsonrpc.QAService._
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import monix.execution.Scheduler.Implicits.global
import mouse.all._

import scala.concurrent.Future

class QAService(
    consensus: Consensus,
    blockchain: Blockchain,
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
      .sendMiner(MineBlocks(req.numBlocks, req.withTransactions, req.parentBlock))
      .map(_ |> (MineBlocksResponse(_)) |> (_.asRight))
      .recover { case t: Throwable =>
        log.info("Unable to mine requested blocks", t)
        Left(JsonRpcError.InternalError)
      }
  }

  def generateCheckpoint(
      req: GenerateCheckpointRequest
  ): ServiceResponse[GenerateCheckpointResponse] = {
    Future {
      val hash = req.blockHash.getOrElse(blockchain.getBestBlock().hash)
      val checkpoint = generateCheckpoint(hash, req.privateKeys)
      syncController ! NewCheckpoint(hash, checkpoint.signatures)
      Right(GenerateCheckpointResponse(checkpoint))
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
    Future {
      Right(GetFederationMembersInfoResponse(blockchainConfig.checkpointPubKeys.toList))
    }
  }
}

object QAService {
  case class MineBlocksRequest(numBlocks: Int, withTransactions: Boolean, parentBlock: Option[ByteString] = None)
  case class MineBlocksResponse(responseType: MinerResponseType, message: Option[String])
  object MineBlocksResponse {
    def apply(minerResponse: MinerResponse): MineBlocksResponse =
      MineBlocksResponse(MinerResponseType(minerResponse), extractMessage(minerResponse))

    private def extractMessage(response: MinerResponse): Option[String] = response match {
      case MinerIsWorking | MiningOrdered | MinerNotExist => None
      case MiningError(msg) => Some(msg)
      case MinerNotSupport(msg) => Some(msg.toString)
    }

    sealed trait MinerResponseType extends EnumEntry
    object MinerResponseType extends Enum[MinerResponseType] {
      val values = findValues

      case object MinerIsWorking extends MinerResponseType
      case object MiningOrdered extends MinerResponseType
      case object MinerNotExist extends MinerResponseType
      case object MiningError extends MinerResponseType
      case object MinerNotSupport extends MinerResponseType

      def apply(minerResponse: MinerResponse): MinerResponseType = minerResponse match {
        case MinerResponses.MinerIsWorking => MinerIsWorking
        case MinerResponses.MiningOrdered => MiningOrdered
        case MinerResponses.MinerNotExist => MinerNotExist
        case MinerResponses.MiningError(_) => MiningError
        case MinerResponses.MinerNotSupport(_) => MinerNotSupport
      }
    }
  }

  case class GenerateCheckpointRequest(privateKeys: Seq[ByteString], blockHash: Option[ByteString])
  case class GenerateCheckpointResponse(checkpoint: Checkpoint)

  case class GetFederationMembersInfoRequest()
  case class GetFederationMembersInfoResponse(membersPublicKeys: Seq[ByteString])
}
