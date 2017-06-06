package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.jsonrpc.Web3Service._
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.JsonDSL._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object JsonRpcController {

  trait JsonDecoder[T] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, T]
  }

  trait JsonEncoder[T] {
    def encodeJson(t: T): JValue
  }


  trait JsonRpcConfig {
    def apis: Seq[String]
  }

  object Apis {
    val Eth = "eth"
    val Web3 = "web3"
    val Net = "net"
    val Db = "db"
    val Personal = "personal"
    val Admin = "admin"
    val Debug = "debug"
  }

}

class JsonRpcController(
  web3Service: Web3Service,
  netService: NetService,
  ethService: EthService,
  personalService: PersonalService,
  config: JsonRpcConfig) {

  import JsonRpcController._
  import EthJsonMethodsImplicits._
  import JsonMethodsImplicits._
  import JsonRpcErrors._

  val apisHandleFns: Map[String, PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]]] = Map(
    Apis.Eth -> handleEthRequest,
    Apis.Web3 -> handleWeb3Request,
    Apis.Net -> handleNetRequest,
    Apis.Db -> PartialFunction.empty,
    Apis.Personal -> handlePersonalRequest,
    Apis.Admin -> PartialFunction.empty,
    Apis.Debug -> PartialFunction.empty
  )

  private def handleWeb3Request: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "web3_sha3", _, _) =>
      handle[Sha3Request, Sha3Response](web3Service.sha3, req)
    case req @ JsonRpcRequest(_, "web3_clientVersion", _, _) =>
      handle[ClientVersionRequest, ClientVersionResponse](web3Service.clientVersion, req)
  }

  private def handleNetRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "net_version", _, _) =>
      handle[VersionRequest, VersionResponse](netService.version, req)
    case req @ JsonRpcRequest(_, "net_listening", _, _) =>
      handle[ListeningRequest, ListeningResponse](netService.listening, req)
    case req @ JsonRpcRequest(_, "net_peerCount", _, _) =>
      handle[PeerCountRequest, PeerCountResponse](netService.peerCount, req)
  }

  // scalastyle:off cyclomatic.complexity
  private def handleEthRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "eth_protocolVersion", _, _) =>
      handle[ProtocolVersionRequest, ProtocolVersionResponse](ethService.protocolVersion, req)
    case req @ JsonRpcRequest(_, "eth_syncing", _, _) =>
      handle[SyncingRequest, SyncingResponse](ethService.syncing, req)
    case req @ JsonRpcRequest(_, "eth_submitHashrate", _, _) =>
      handle[SubmitHashRateRequest, SubmitHashRateResponse](ethService.submitHashRate, req)
    case req @ JsonRpcRequest(_, "eth_hashrate", _, _) =>
      handle[GetHashRateRequest, GetHashRateResponse](ethService.getHashRate, req)
    case req @ JsonRpcRequest(_, "eth_gasPrice", _, _) =>
      handle[GetGasPriceRequest, GetGasPriceResponse](ethService.getGetGasPrice, req)
    case req @ JsonRpcRequest(_, "eth_getWork", _, _) =>
      handle[GetWorkRequest, GetWorkResponse](ethService.getWork, req)
    case req @ JsonRpcRequest(_, "eth_submitWork", _, _) =>
      handle[SubmitWorkRequest, SubmitWorkResponse](ethService.submitWork, req)
    case req @ JsonRpcRequest(_, "eth_blockNumber", _, _) =>
      handle[BestBlockNumberRequest, BestBlockNumberResponse](ethService.bestBlockNumber, req)
    case req @ JsonRpcRequest(_, "eth_coinbase", _, _) =>
      handle[GetCoinbaseRequest, GetCoinbaseResponse](ethService.getCoinbase, req)
    case req @ JsonRpcRequest(_, "eth_getBlockTransactionCountByHash", _, _) =>
      handle[TxCountByBlockHashRequest, TxCountByBlockHashResponse](ethService.getBlockTransactionCountByHash, req)
    case req @ JsonRpcRequest(_, "eth_getBlockByHash", _, _) =>
      handle[BlockByBlockHashRequest, BlockByBlockHashResponse](ethService.getByBlockHash, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionByBlockHashAndIndex", _, _) =>
      handle[GetTransactionByBlockHashAndIndexRequest, GetTransactionByBlockHashAndIndexResponse](ethService.getTransactionByBlockHashAndIndexRequest, req)
    case req @ JsonRpcRequest(_, "eth_getUncleByBlockHashAndIndex", _, _) =>
      handle[UncleByBlockHashAndIndexRequest, UncleByBlockHashAndIndexResponse](ethService.getUncleByBlockHashAndIndex, req)
    case req @ JsonRpcRequest(_, "eth_accounts", _, _) =>
      handle[ListAccountsRequest, ListAccountsResponse](personalService.listAccounts, req)
    case req @ JsonRpcRequest(_, "eth_sendRawTransaction", _, _) =>
      handle[SendRawTransactionRequest, SendRawTransactionResponse](ethService.sendRawTransaction, req)
    case req @ JsonRpcRequest(_, "eth_sendTransaction", _, _) =>
      handle[SendTransactionRequest, SendTransactionResponse](personalService.sendTransaction, req)
    case req @ JsonRpcRequest(_, "eth_call", _, _) =>
      handle[CallRequest, CallResponse](ethService.call, req)
    case req @ JsonRpcRequest(_, "eth_getCode", _, _) =>
      handle[GetCodeRequest, GetCodeResponse](ethService.getCode, req)
    case req @ JsonRpcRequest(_, "eth_getUncleCountByBlockNumber", _, _) =>
      handle[GetUncleCountByBlockNumberRequest, GetUncleCountByBlockNumberResponse](ethService.getUncleCountByBlockNumber, req)
    case req @ JsonRpcRequest(_, "eth_getUncleCountByBlockHash", _, _) =>
      handle[GetUncleCountByBlockHashRequest, GetUncleCountByBlockHashResponse](ethService.getUncleCountByBlockHash, req)
    case req @ JsonRpcRequest(_, "eth_getBlockTransactionCountByNumber", _, _) =>
      handle[GetBlockTransactionCountByNumberRequest, GetBlockTransactionCountByNumberResponse](ethService.getBlockTransactionCountByNumber, req)
  }

  private def handlePersonalRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "personal_importRawKey", _, _) =>
      handle[ImportRawKeyRequest, ImportRawKeyResponse](personalService.importRawKey, req)

    case req @ JsonRpcRequest(_, "personal_newAccount", _, _) =>
      handle[NewAccountRequest, NewAccountResponse](personalService.newAccount, req)

    case req @ JsonRpcRequest(_, "personal_listAccounts", _, _) =>
      handle[ListAccountsRequest, ListAccountsResponse](personalService.listAccounts, req)

    case req @ JsonRpcRequest(_, "personal_sendTransaction" | "personal_signAndSendTransaction", _, _) =>
      handle[SendTransactionWithPassphraseRequest, SendTransactionWithPassphraseResponse](personalService.sendTransaction, req)

    case req @ JsonRpcRequest(_, "personal_unlockAccount", _, _) =>
      handle[UnlockAccountRequest, UnlockAccountResponse](personalService.unlockAccount, req)

    case req @ JsonRpcRequest(_, "personal_lockAccount", _, _) =>
      handle[LockAccountRequest, LockAccountResponse](personalService.lockAccount, req)
  }

  def handleRequest(request: JsonRpcRequest): Future[JsonRpcResponse] = {
    val notFoundFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
      case _ => Future.successful(errorResponse(request, MethodNotFound))
    }

    val handleFn = config.apis.foldLeft(notFoundFn)((fn, api) => apisHandleFns.getOrElse(api, PartialFunction.empty) orElse fn)

    handleFn(request)
  }

  private def handle[Req, Res](fn: Req => Future[Either[JsonRpcError, Res]], rpcReq: JsonRpcRequest)
                              (implicit dec: JsonDecoder[Req], enc: JsonEncoder[Res]): Future[JsonRpcResponse] = {
    dec.decodeJson(rpcReq.params) match {
      case Right(req) =>
        fn(req)
          .map {
            case Right(success) => successResponse(rpcReq, success)
            case Left(error) => errorResponse(rpcReq, error)
          }
          .recover { case ex => errorResponse(rpcReq, InternalError) }
      case Left(error) =>
        Future.successful(errorResponse(rpcReq, error))
    }
  }

  private def successResponse[T](req: JsonRpcRequest, result: T)(implicit enc: JsonEncoder[T]): JsonRpcResponse =
    JsonRpcResponse(req.jsonrpc, Some(enc.encodeJson(result)), None, req.id.getOrElse(0))

  private def errorResponse[T](req: JsonRpcRequest, error: JsonRpcError): JsonRpcResponse =
    JsonRpcResponse(req.jsonrpc, None, Some(error), req.id.getOrElse(0))

}
