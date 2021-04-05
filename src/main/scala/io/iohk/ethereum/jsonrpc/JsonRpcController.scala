package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.CheckpointingService._
import io.iohk.ethereum.jsonrpc.DebugService.{ListPeersInfoRequest, ListPeersInfoResponse}
import io.iohk.ethereum.jsonrpc.EthBlocksService._
import io.iohk.ethereum.jsonrpc.EthInfoService._
import io.iohk.ethereum.jsonrpc.EthTxService._
import io.iohk.ethereum.jsonrpc.EthUserService._
import io.iohk.ethereum.jsonrpc.EthFilterService._
import io.iohk.ethereum.jsonrpc.MantisService.{GetAccountTransactionsRequest, GetAccountTransactionsResponse}
import io.iohk.ethereum.jsonrpc.EthMiningService._
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

case class JsonRpcController(
    web3Service: Web3Service,
    netService: NetService,
    ethInfoService: EthInfoService,
    ethMiningService: EthMiningService,
    ethBlocksService: EthBlocksService,
    ethTxService: EthTxService,
    ethUserService: EthUserService,
    ethFilterService: EthFilterService,
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
  import EthBlocksJsonMethodsImplicits._
  import EthMiningJsonMethodsImplicits._
  import EthTxJsonMethodsImplicits._
  import EthUserJsonMethodsImplicits._
  import EthFilterJsonMethodsImplicits._
  import IeleJsonMethodsImplicits._
  import EthProofJsonMethodsImplicits._
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
      handle[ProtocolVersionRequest, ProtocolVersionResponse](ethInfoService.protocolVersion, req)
    case req @ JsonRpcRequest(_, "eth_chainId", _, _) =>
      handle[ChainIdRequest, ChainIdResponse](ethInfoService.chainId, req)
    case req @ JsonRpcRequest(_, "eth_syncing", _, _) =>
      handle[SyncingRequest, SyncingResponse](ethInfoService.syncing, req)
    case req @ JsonRpcRequest(_, "eth_submitHashrate", _, _) =>
      handle[SubmitHashRateRequest, SubmitHashRateResponse](ethMiningService.submitHashRate, req)
    case req @ JsonRpcRequest(_, "eth_hashrate", _, _) =>
      handle[GetHashRateRequest, GetHashRateResponse](ethMiningService.getHashRate, req)
    case req @ JsonRpcRequest(_, "eth_gasPrice", _, _) =>
      handle[GetGasPriceRequest, GetGasPriceResponse](ethTxService.getGetGasPrice, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionByBlockNumberAndIndex", _, _) =>
      handle[GetTransactionByBlockNumberAndIndexRequest, GetTransactionByBlockNumberAndIndexResponse](
        ethTxService.getTransactionByBlockNumberAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_mining", _, _) =>
      handle[GetMiningRequest, GetMiningResponse](ethMiningService.getMining, req)
    case req @ JsonRpcRequest(_, "eth_getWork", _, _) =>
      handle[GetWorkRequest, GetWorkResponse](ethMiningService.getWork, req)
    case req @ JsonRpcRequest(_, "eth_submitWork", _, _) =>
      handle[SubmitWorkRequest, SubmitWorkResponse](ethMiningService.submitWork, req)
    case req @ JsonRpcRequest(_, "eth_blockNumber", _, _) =>
      handle[BestBlockNumberRequest, BestBlockNumberResponse](ethBlocksService.bestBlockNumber, req)
    case req @ JsonRpcRequest(_, "eth_coinbase", _, _) =>
      handle[GetCoinbaseRequest, GetCoinbaseResponse](ethMiningService.getCoinbase, req)
    case req @ JsonRpcRequest(_, "eth_getBlockTransactionCountByHash", _, _) =>
      handle[TxCountByBlockHashRequest, TxCountByBlockHashResponse](
        ethBlocksService.getBlockTransactionCountByHash,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getBlockByHash", _, _) =>
      handle[BlockByBlockHashRequest, BlockByBlockHashResponse](ethBlocksService.getByBlockHash, req)
    case req @ JsonRpcRequest(_, "eth_getBlockByNumber", _, _) =>
      handle[BlockByNumberRequest, BlockByNumberResponse](ethBlocksService.getBlockByNumber, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionByBlockHashAndIndex", _, _) =>
      handle[GetTransactionByBlockHashAndIndexRequest, GetTransactionByBlockHashAndIndexResponse](
        ethTxService.getTransactionByBlockHashAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getUncleByBlockHashAndIndex", _, _) =>
      handle[UncleByBlockHashAndIndexRequest, UncleByBlockHashAndIndexResponse](
        ethBlocksService.getUncleByBlockHashAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getUncleByBlockNumberAndIndex", _, _) =>
      handle[UncleByBlockNumberAndIndexRequest, UncleByBlockNumberAndIndexResponse](
        ethBlocksService.getUncleByBlockNumberAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_accounts", _, _) =>
      handle[ListAccountsRequest, ListAccountsResponse](personalService.listAccounts, req)
    case req @ JsonRpcRequest(_, "eth_sendRawTransaction", _, _) =>
      handle[SendRawTransactionRequest, SendRawTransactionResponse](ethTxService.sendRawTransaction, req)
    case req @ JsonRpcRequest(_, "eth_sendTransaction", _, _) =>
      handle[SendTransactionRequest, SendTransactionResponse](personalService.sendTransaction, req)
    case req @ JsonRpcRequest(_, "eth_call", _, _) =>
      handle[CallRequest, CallResponse](ethInfoService.call, req)(eth_call, eth_call)
    case req @ JsonRpcRequest(_, "eth_estimateGas", _, _) =>
      handle[CallRequest, EstimateGasResponse](ethInfoService.estimateGas, req)(eth_estimateGas, eth_estimateGas)
    case req @ JsonRpcRequest(_, "eth_getCode", _, _) =>
      handle[GetCodeRequest, GetCodeResponse](ethUserService.getCode, req)
    case req @ JsonRpcRequest(_, "eth_getUncleCountByBlockNumber", _, _) =>
      handle[GetUncleCountByBlockNumberRequest, GetUncleCountByBlockNumberResponse](
        ethBlocksService.getUncleCountByBlockNumber,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getUncleCountByBlockHash", _, _) =>
      handle[GetUncleCountByBlockHashRequest, GetUncleCountByBlockHashResponse](
        ethBlocksService.getUncleCountByBlockHash,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getBlockTransactionCountByNumber", _, _) =>
      handle[GetBlockTransactionCountByNumberRequest, GetBlockTransactionCountByNumberResponse](
        ethBlocksService.getBlockTransactionCountByNumber,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getBalance", _, _) =>
      handle[GetBalanceRequest, GetBalanceResponse](ethUserService.getBalance, req)
    case req @ JsonRpcRequest(_, "eth_getStorageAt", _, _) =>
      handle[GetStorageAtRequest, GetStorageAtResponse](ethUserService.getStorageAt, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionCount", _, _) =>
      handle[GetTransactionCountRequest, GetTransactionCountResponse](ethUserService.getTransactionCount, req)
    case req @ JsonRpcRequest(_, "eth_newFilter", _, _) =>
      handle[NewFilterRequest, NewFilterResponse](ethFilterService.newFilter, req)
    case req @ JsonRpcRequest(_, "eth_newBlockFilter", _, _) =>
      handle[NewBlockFilterRequest, NewFilterResponse](ethFilterService.newBlockFilter, req)
    case req @ JsonRpcRequest(_, "eth_newPendingTransactionFilter", _, _) =>
      handle[NewPendingTransactionFilterRequest, NewFilterResponse](ethFilterService.newPendingTransactionFilter, req)
    case req @ JsonRpcRequest(_, "eth_uninstallFilter", _, _) =>
      handle[UninstallFilterRequest, UninstallFilterResponse](ethFilterService.uninstallFilter, req)
    case req @ JsonRpcRequest(_, "eth_getFilterChanges", _, _) =>
      handle[GetFilterChangesRequest, GetFilterChangesResponse](ethFilterService.getFilterChanges, req)
    case req @ JsonRpcRequest(_, "eth_getFilterLogs", _, _) =>
      handle[GetFilterLogsRequest, GetFilterLogsResponse](ethFilterService.getFilterLogs, req)
    case req @ JsonRpcRequest(_, "eth_getLogs", _, _) =>
      handle[GetLogsRequest, GetLogsResponse](ethFilterService.getLogs, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionByHash", _, _) =>
      handle[GetTransactionByHashRequest, GetTransactionByHashResponse](ethTxService.getTransactionByHash, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionReceipt", _, _) =>
      handle[GetTransactionReceiptRequest, GetTransactionReceiptResponse](ethTxService.getTransactionReceipt, req)
    case req @ JsonRpcRequest(_, "eth_sign", _, _) =>
      // Even if it's under eth_xxx this method actually does the same as personal_sign but needs the account
      // to be unlocked before calling
      handle[SignRequest, SignResponse](personalService.sign, req)(eth_sign, personal_sign)
    case req @ JsonRpcRequest(_, "eth_getStorageRoot", _, _) =>
      handle[GetStorageRootRequest, GetStorageRootResponse](ethUserService.getStorageRoot, req)
    case req @ JsonRpcRequest(_, "eth_getRawTransactionByHash", _, _) =>
      handle[GetTransactionByHashRequest, RawTransactionResponse](ethTxService.getRawTransactionByHash, req)
    case req @ JsonRpcRequest(_, "eth_getRawTransactionByBlockHashAndIndex", _, _) =>
      handle[GetTransactionByBlockHashAndIndexRequest, RawTransactionResponse](
        ethTxService.getRawTransactionByBlockHashAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_getRawTransactionByBlockNumberAndIndex", _, _) =>
      handle[GetTransactionByBlockNumberAndIndexRequest, RawTransactionResponse](
        ethTxService.getRawTransactionByBlockNumberAndIndex,
        req
      )
    case req @ JsonRpcRequest(_, "eth_pendingTransactions", _, _) =>
      handle[EthPendingTransactionsRequest, EthPendingTransactionsResponse](ethTxService.ethPendingTransactions, req)
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
    case req @ JsonRpcRequest(_, "test_importRawBlock", _, _) =>
      handle[ImportRawBlockRequest, ImportRawBlockResponse](testService.importRawBlock, req)
    case req @ JsonRpcRequest(_, "miner_setEtherbase", _, _) =>
      handle[SetEtherbaseRequest, SetEtherbaseResponse](testService.setEtherbase, req)
    case req @ JsonRpcRequest(_, "debug_accountRange", _, _) =>
      handle[AccountsInRangeRequest, AccountsInRangeResponse](testService.getAccountsInRange, req)
    case req @ JsonRpcRequest(_, "debug_storageRangeAt", _, _) =>
      handle[StorageRangeRequest, StorageRangeResponse](testService.storageRangeAt, req)
  }

  private def handleIeleRequest: PartialFunction[JsonRpcRequest, Task[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "iele_sendTransaction", _, _) =>
      handle[SendIeleTransactionRequest, SendTransactionResponse](personalService.sendIeleTransaction, req)
    case req @ JsonRpcRequest(_, "iele_call", _, _) =>
      handle[IeleCallRequest, IeleCallResponse](ethInfoService.ieleCall, req)
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
