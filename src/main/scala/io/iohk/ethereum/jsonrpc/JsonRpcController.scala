package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.CheckpointingService._
import io.iohk.ethereum.jsonrpc.DebugService.{ListPeersInfoRequest, ListPeersInfoResponse}
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.MantisService.{GetAccountTransactionsRequest, GetAccountTransactionsResponse}
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.jsonrpc.ProofService.{GetProofRequest, GetProofResponse}
import io.iohk.ethereum.jsonrpc.QAService.{
  GenerateCheckpointRequest,
  GenerateCheckpointResponse,
  GetFederationMembersInfoRequest,
  GetFederationMembersInfoResponse
}
import io.iohk.ethereum.jsonrpc.TestService._
import io.iohk.ethereum.jsonrpc.Web3Service._
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.utils.Logger
import monix.eval.Task
import org.json4s.JsonDSL._

class JsonRpcController(
    web3Service: Web3Service,
    netService: NetService,
    ethService: EthService,
    personalService: PersonalService,
    testServiceOpt: Option[TestService],
    debugService: DebugService,
    qaService: QAService,
    checkpointingService: CheckpointingService,
    mantisService: MantisService,
    proofService: ProofService,
    override val config: JsonRpcConfig
) extends ApisBuilder
    with Logger
    with JsonRpcBaseController {

  import CheckpointingJsonMethodsImplicits._
  import DebugJsonMethodsImplicits._
  import EthJsonMethodsImplicits._
  import IeleJsonMethodsImplicits._
  import JsonMethodsImplicits._
  import QAJsonMethodsImplicits._
  import TestJsonMethodsImplicits._
  import MantisJsonMethodImplicits._

  override def apisHandleFns: Map[String, PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]]] = Map(
    Apis.Eth -> handleEthRequest,
    Apis.Web3 -> handleWeb3Request,
    Apis.Net -> handleNetRequest,
    Apis.Personal -> handlePersonalRequest,
    Apis.Mantis -> handleMantisRequest,
    Apis.Rpc -> handleRpcRequest,
    Apis.Debug -> handleDebugRequest,
    Apis.Test -> handleTestRequest,
    Apis.Iele -> handleIeleRequest,
    Apis.Qa -> handleQARequest,
    Apis.Checkpointing -> handleCheckpointingRequest
  )

  override def enabledApis: Seq[String] = config.apis :+ Apis.Rpc // RPC enabled by default

  private def handleWeb3Request: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "web3_sha3", _, _) =>
      handle[Sha3Request, Sha3Response](web3Service.sha3, req)
    case req @ JsonRpcRequest(_, "web3_clientVersion", _, _) =>
      handle[ClientVersionRequest, ClientVersionResponse](web3Service.clientVersion, req)
  }

  private def handleNetRequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "net_version", _, _) =>
      handle[VersionRequest, VersionResponse](netService.version, req)
    case req @ JsonRpcRequest(_, "net_listening", _, _) =>
      handle[ListeningRequest, ListeningResponse](netService.listening, req)
    case req @ JsonRpcRequest(_, "net_peerCount", _, _) =>
      handle[PeerCountRequest, PeerCountResponse](netService.peerCount, req)
  }

  // scalastyle:off cyclomatic.complexity
  // scalastyle:off method.length
  private def handleEthRequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "eth_protocolVersion", _, _) =>
      handle[ProtocolVersionRequest, ProtocolVersionResponse](ethService.protocolVersion, req)
    case req @ JsonRpcRequest(_, "eth_chainId", _, _) =>
      handle[ChainIdRequest, ChainIdResponse](ethService.chainId, req)
    case req @ JsonRpcRequest(_, "eth_syncing", _, _) =>
      handle[SyncingRequest, SyncingResponse](ethService.syncing, req)
    case req @ JsonRpcRequest(_, "eth_submitHashrate", _, _) =>
      handle[SubmitHashRateRequest, SubmitHashRateResponse](ethService.submitHashRate, req)
    case req @ JsonRpcRequest(_, "eth_hashrate", _, _) =>
      handle[GetHashRateRequest, GetHashRateResponse](ethService.getHashRate, req)
    case req @ JsonRpcRequest(_, "eth_gasPrice", _, _) =>
      handle[GetGasPriceRequest, GetGasPriceResponse](ethService.getGetGasPrice, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionByBlockNumberAndIndex", _, _) =>
      handle[GetTransactionByBlockNumberAndIndexRequest, GetTransactionByBlockNumberAndIndexResponse](
        ethService.getTransactionByBlockNumberAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_mining", _, _) =>
      handle[GetMiningRequest, GetMiningResponse](ethService.getMining, req)
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
    case req @ JsonRpcRequest(_, "eth_getBlockByNumber", _, _) =>
      handle[BlockByNumberRequest, BlockByNumberResponse](ethService.getBlockByNumber, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionByBlockHashAndIndex", _, _) =>
      handle[GetTransactionByBlockHashAndIndexRequest, GetTransactionByBlockHashAndIndexResponse](
        ethService.getTransactionByBlockHashAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getUncleByBlockHashAndIndex", _, _) =>
      handle[UncleByBlockHashAndIndexRequest, UncleByBlockHashAndIndexResponse](
        ethService.getUncleByBlockHashAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getUncleByBlockNumberAndIndex", _, _) =>
      handle[UncleByBlockNumberAndIndexRequest, UncleByBlockNumberAndIndexResponse](
        ethService.getUncleByBlockNumberAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_accounts", _, _) =>
      handle[ListAccountsRequest, ListAccountsResponse](personalService.listAccounts, req)
    case req @ JsonRpcRequest(_, "eth_sendRawTransaction", _, _) =>
      handle[SendRawTransactionRequest, SendRawTransactionResponse](ethService.sendRawTransaction, req)
    case req @ JsonRpcRequest(_, "eth_sendTransaction", _, _) =>
      handle[SendTransactionRequest, SendTransactionResponse](personalService.sendTransaction, req)
    case req @ JsonRpcRequest(_, "eth_call", _, _) =>
      handle[CallRequest, CallResponse](ethService.call, req)(eth_call, eth_call)
    case req @ JsonRpcRequest(_, "eth_estimateGas", _, _) =>
      handle[CallRequest, EstimateGasResponse](ethService.estimateGas, req)(eth_estimateGas, eth_estimateGas)
    case req @ JsonRpcRequest(_, "eth_getCode", _, _) =>
      handle[GetCodeRequest, GetCodeResponse](ethService.getCode, req)
    case req @ JsonRpcRequest(_, "eth_getUncleCountByBlockNumber", _, _) =>
      handle[GetUncleCountByBlockNumberRequest, GetUncleCountByBlockNumberResponse](
        ethService.getUncleCountByBlockNumber,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getUncleCountByBlockHash", _, _) =>
      handle[GetUncleCountByBlockHashRequest, GetUncleCountByBlockHashResponse](
        ethService.getUncleCountByBlockHash,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getBlockTransactionCountByNumber", _, _) =>
      handle[GetBlockTransactionCountByNumberRequest, GetBlockTransactionCountByNumberResponse](
        ethService.getBlockTransactionCountByNumber,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getBalance", _, _) =>
      handle[GetBalanceRequest, GetBalanceResponse](ethService.getBalance, req)
    case req @ JsonRpcRequest(_, "eth_getStorageAt", _, _) =>
      handle[GetStorageAtRequest, GetStorageAtResponse](ethService.getStorageAt, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionCount", _, _) =>
      handle[GetTransactionCountRequest, GetTransactionCountResponse](ethService.getTransactionCount, req)
    case req @ JsonRpcRequest(_, "eth_newFilter", _, _) =>
      handle[NewFilterRequest, NewFilterResponse](ethService.newFilter, req)
    case req @ JsonRpcRequest(_, "eth_newBlockFilter", _, _) =>
      handle[NewBlockFilterRequest, NewFilterResponse](ethService.newBlockFilter, req)
    case req @ JsonRpcRequest(_, "eth_newPendingTransactionFilter", _, _) =>
      handle[NewPendingTransactionFilterRequest, NewFilterResponse](ethService.newPendingTransactionFilter, req)
    case req @ JsonRpcRequest(_, "eth_uninstallFilter", _, _) =>
      handle[UninstallFilterRequest, UninstallFilterResponse](ethService.uninstallFilter, req)
    case req @ JsonRpcRequest(_, "eth_getFilterChanges", _, _) =>
      handle[GetFilterChangesRequest, GetFilterChangesResponse](ethService.getFilterChanges, req)
    case req @ JsonRpcRequest(_, "eth_getFilterLogs", _, _) =>
      handle[GetFilterLogsRequest, GetFilterLogsResponse](ethService.getFilterLogs, req)
    case req @ JsonRpcRequest(_, "eth_getLogs", _, _) =>
      handle[GetLogsRequest, GetLogsResponse](ethService.getLogs, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionByHash", _, _) =>
      handle[GetTransactionByHashRequest, GetTransactionByHashResponse](ethService.getTransactionByHash, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionReceipt", _, _) =>
      handle[GetTransactionReceiptRequest, GetTransactionReceiptResponse](ethService.getTransactionReceipt, req)
    case req @ JsonRpcRequest(_, "eth_sign", _, _) =>
      // Even if it's under eth_xxx this method actually does the same as personal_sign but needs the account
      // to be unlocked before calling
      handle[SignRequest, SignResponse](personalService.sign, req)(eth_sign, personal_sign)
    case req @ JsonRpcRequest(_, "eth_getStorageRoot", _, _) =>
      handle[GetStorageRootRequest, GetStorageRootResponse](ethService.getStorageRoot, req)
    case req @ JsonRpcRequest(_, "eth_getRawTransactionByHash", _, _) =>
      handle[GetTransactionByHashRequest, RawTransactionResponse](ethService.getRawTransactionByHash, req)
    case req @ JsonRpcRequest(_, "eth_getRawTransactionByBlockHashAndIndex", _, _) =>
      handle[GetTransactionByBlockHashAndIndexRequest, RawTransactionResponse](
        ethService.getRawTransactionByBlockHashAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getRawTransactionByBlockNumberAndIndex", _, _) =>
      handle[GetTransactionByBlockNumberAndIndexRequest, RawTransactionResponse](
        ethService.getRawTransactionByBlockNumberAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_pendingTransactions", _, _) =>
      handle[EthPendingTransactionsRequest, EthPendingTransactionsResponse](ethService.ethPendingTransactions, req)
    case req @ JsonRpcRequest(_, "eth_getProof", _, _) =>
      handle[GetProofRequest, GetProofResponse](proofService.getProof, req)
  }

  private def handleDebugRequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "debug_listPeersInfo", _, _) =>
      handle[ListPeersInfoRequest, ListPeersInfoResponse](debugService.listPeersInfo, req)
  }

  private def handleTestRequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    testServiceOpt match {
      case Some(testService) => handleTestRequest(testService)
      case None => PartialFunction.empty
    }
  }

  private def handleTestRequest(testService: TestService): PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "test_setChainParams", _, _) =>
      handle[SetChainParamsRequest, SetChainParamsResponse](testService.setChainParams, req)
    case req @ JsonRpcRequest(_, "test_mineBlocks", _, _) =>
      handle[MineBlocksRequest, MineBlocksResponse](testService.mineBlocks, req)
    case req @ JsonRpcRequest(_, "test_modifyTimestamp", _, _) =>
      handle[ModifyTimestampRequest, ModifyTimestampResponse](testService.modifyTimestamp, req)
    case req @ JsonRpcRequest(_, "test_rewindToBlock", _, _) =>
      handle[RewindToBlockRequest, RewindToBlockResponse](testService.rewindToBlock, req)
    case req @ JsonRpcRequest(_, "miner_setEtherbase", _, _) =>
      handle[SetEtherbaseRequest, SetEtherbaseResponse](testService.setEtherbase, req)
  }

  private def handleIeleRequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "iele_sendTransaction", _, _) =>
      handle[SendIeleTransactionRequest, SendTransactionResponse](personalService.sendIeleTransaction, req)
    case req @ JsonRpcRequest(_, "iele_call", _, _) =>
      handle[IeleCallRequest, IeleCallResponse](ethService.ieleCall, req)
  }

  private def handlePersonalRequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "personal_importRawKey", _, _) =>
      handle[ImportRawKeyRequest, ImportRawKeyResponse](personalService.importRawKey, req)

    case req @ JsonRpcRequest(_, "personal_newAccount", _, _) =>
      handle[NewAccountRequest, NewAccountResponse](personalService.newAccount, req)

    case req @ JsonRpcRequest(_, "personal_listAccounts", _, _) =>
      handle[ListAccountsRequest, ListAccountsResponse](personalService.listAccounts, req)

    case req @ JsonRpcRequest(_, "personal_sendTransaction" | "personal_signAndSendTransaction", _, _) =>
      handle[SendTransactionWithPassphraseRequest, SendTransactionWithPassphraseResponse](
        personalService.sendTransaction,
        req
      )

    case req @ JsonRpcRequest(_, "personal_unlockAccount", _, _) =>
      handle[UnlockAccountRequest, UnlockAccountResponse](personalService.unlockAccount, req)

    case req @ JsonRpcRequest(_, "personal_lockAccount", _, _) =>
      handle[LockAccountRequest, LockAccountResponse](personalService.lockAccount, req)

    case req @ JsonRpcRequest(_, "personal_sign", _, _) =>
      handle[SignRequest, SignResponse](personalService.sign, req)(personal_sign, personal_sign)

    case req @ JsonRpcRequest(_, "personal_ecRecover", _, _) =>
      handle[EcRecoverRequest, EcRecoverResponse](personalService.ecRecover, req)
  }

  private def handleMantisRequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "mantis_getAccountTransactions", _, _) =>
      handle[GetAccountTransactionsRequest, GetAccountTransactionsResponse](mantisService.getAccountTransactions, req)
  }

  private def handleQARequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "qa_mineBlocks", _, _) =>
      handle[QAService.MineBlocksRequest, QAService.MineBlocksResponse](qaService.mineBlocks, req)

    case req @ JsonRpcRequest(_, "qa_generateCheckpoint", _, _) =>
      handle[GenerateCheckpointRequest, GenerateCheckpointResponse](qaService.generateCheckpoint, req)

    case req @ JsonRpcRequest(_, "qa_getFederationMembersInfo", _, _) =>
      handle[GetFederationMembersInfoRequest, GetFederationMembersInfoResponse](qaService.getFederationMembersInfo, req)
  }

  private def handleCheckpointingRequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "checkpointing_getLatestBlock", _, _) =>
      handle[GetLatestBlockRequest, GetLatestBlockResponse](checkpointingService.getLatestBlock, req)

    case req @ JsonRpcRequest(_, "checkpointing_pushCheckpoint", _, _) =>
      handle[PushCheckpointRequest, PushCheckpointResponse](checkpointingService.pushCheckpoint, req)
  }

  private def handleRpcRequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "rpc_modules", _, _) =>
      val result = enabledApis.map { _ -> "1.0" }.toMap
      Task(JsonRpcResponse("2.0", Some(result), None, req.id))
  }
}
