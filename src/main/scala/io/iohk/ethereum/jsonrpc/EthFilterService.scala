package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.ByteString
import akka.util.Timeout
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.{FilterManager => FM}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import io.iohk.ethereum.jsonrpc.FilterManager.FilterChanges
import io.iohk.ethereum.jsonrpc.FilterManager.FilterLogs
import io.iohk.ethereum.jsonrpc.FilterManager.LogFilterLogs
import io.iohk.ethereum.utils._
import monix.eval.Task

object EthFilterService {
  case class NewFilterRequest(filter: Filter)
  case class Filter(
      fromBlock: Option[BlockParam],
      toBlock: Option[BlockParam],
      address: Option[Address],
      topics: Seq[Seq[ByteString]]
  )

  case class NewBlockFilterRequest()
  case class NewPendingTransactionFilterRequest()

  case class NewFilterResponse(filterId: BigInt)

  case class UninstallFilterRequest(filterId: BigInt)
  case class UninstallFilterResponse(success: Boolean)

  case class GetFilterChangesRequest(filterId: BigInt)
  case class GetFilterChangesResponse(filterChanges: FilterChanges)

  case class GetFilterLogsRequest(filterId: BigInt)
  case class GetFilterLogsResponse(filterLogs: FilterLogs)

  case class GetLogsRequest(filter: Filter)
  case class GetLogsResponse(filterLogs: LogFilterLogs)
}

class EthFilterService(
    filterManager: ActorRef,
    filterConfig: FilterConfig
) {
  import EthFilterService._
  implicit val timeout: Timeout = Timeout(filterConfig.filterManagerQueryTimeout)

  def newFilter(req: NewFilterRequest): ServiceResponse[NewFilterResponse] = {
    import req.filter._

    filterManager
      .askFor[FM.NewFilterResponse](FM.NewLogFilter(fromBlock, toBlock, address, topics))
      .map { resp =>
        Right(NewFilterResponse(resp.id))
      }
  }

  def newBlockFilter(req: NewBlockFilterRequest): ServiceResponse[NewFilterResponse] =
    filterManager
      .askFor[FM.NewFilterResponse](FM.NewBlockFilter)
      .map { resp =>
        Right(NewFilterResponse(resp.id))
      }

  def newPendingTransactionFilter(req: NewPendingTransactionFilterRequest): ServiceResponse[NewFilterResponse] =
    filterManager
      .askFor[FM.NewFilterResponse](FM.NewPendingTransactionFilter)
      .map { resp =>
        Right(NewFilterResponse(resp.id))
      }

  def uninstallFilter(req: UninstallFilterRequest): ServiceResponse[UninstallFilterResponse] =
    filterManager
      .askFor[FM.UninstallFilterResponse.type](FM.UninstallFilter(req.filterId))
      .map(_ => Right(UninstallFilterResponse(success = true)))

  def getFilterChanges(req: GetFilterChangesRequest): ServiceResponse[GetFilterChangesResponse] =
    filterManager
      .askFor[FM.FilterChanges](FM.GetFilterChanges(req.filterId))
      .map { filterChanges =>
        Right(GetFilterChangesResponse(filterChanges))
      }

  def getFilterLogs(req: GetFilterLogsRequest): ServiceResponse[GetFilterLogsResponse] =
    filterManager
      .askFor[FM.FilterLogs](FM.GetFilterLogs(req.filterId))
      .map { filterLogs =>
        Right(GetFilterLogsResponse(filterLogs))
      }

  def getLogs(req: GetLogsRequest): ServiceResponse[GetLogsResponse] = {
    import req.filter._

    filterManager
      .askFor[FM.LogFilterLogs](FM.GetLogs(fromBlock, toBlock, address, topics))
      .map { filterLogs =>
        Right(GetLogsResponse(filterLogs))
      }
  }
}
