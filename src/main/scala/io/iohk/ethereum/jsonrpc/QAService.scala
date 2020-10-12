package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import akka.util.ByteString
import cats.implicits._
import enumeratum._
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.consensus.ethash.MinerResponses._
import io.iohk.ethereum.consensus.ethash.MockedMinerProtocol.MineBlocks
import io.iohk.ethereum.consensus.ethash.{MinerResponse, MinerResponses}
import io.iohk.ethereum.jsonrpc.QAService.MineBlocksResponse.MinerResponseType
import io.iohk.ethereum.jsonrpc.QAService.{
  MineBlocksRequest,
  MineBlocksResponse
}
import io.iohk.ethereum.utils.Logger
import monix.execution.Scheduler.Implicits.global
import mouse.all._

class QAService(
    consensus: Consensus
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
      .recover {
        case t: Throwable =>
          log.info("Unable to mine requested blocks", t)
          Left(JsonRpcErrors.InternalError)
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
}
