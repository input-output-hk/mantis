package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.consensus.ethash.MockedMinerProtocol.MineBlocks
import io.iohk.ethereum.consensus.ethash.{MinerResponse, MinerResponses}
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Address, SignedTransactionWithSender, Transaction}
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.QAService.MineBlocksResponse.MinerResponseType._
import io.iohk.ethereum.jsonrpc.QAService.{
  GetPendingTransactionsRequest,
  GetPendingTransactionsResponse,
  MineBlocksRequest,
  MineBlocksResponse
}
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransaction
import io.iohk.ethereum.utils.Config
import org.json4s.JsonAST._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpec}
import scala.concurrent.Future

class QaJRCSpec extends WordSpec with Matchers with ScalaFutures with NormalPatience with JsonMethodsImplicits {

  "QaJRC" should {
    "request block mining and return valid response with correct message" when {
      "mining ordered" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MinerResponses.MiningOrdered)

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveResult(JObject(responseType(MiningOrdered), nullMessage))
      }

      "miner is working" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MinerResponses.MinerIsWorking)

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveResult(JObject(responseType(MinerIsWorking), nullMessage))
      }

      "miner doesn't exist" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MinerResponses.MinerNotExist)

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveResult(JObject(responseType(MinerNotExist), nullMessage))
      }

      "miner not support current msg" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MinerResponses.MinerNotSupport(MineBlocks(1, true)))

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveResult(JObject(responseType(MinerNotSupport), msg("MineBlocks(1,true,None)")))
      }

      "miner return error" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MinerResponses.MiningError("error"))

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveResult(JObject(responseType(MiningError), msg("error")))
      }
    }

    "request block mining and return InternalError" when {
      "communication with miner failed" in new TestSetup {
        (qaService.mineBlocks _)
          .expects(mineBlocksReq)
          .returning(Future.failed(new ClassCastException("error")))

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveError(JsonRpcErrors.InternalError)
      }
    }

    "request pending transactions and return valid response" when {
      "mempool is empty" in new TestSetup {
        (qaService.getPendingTransactions _)
          .expects(getPendingTransactionReq)
          .returning(Future.successful(Right(GetPendingTransactionsResponse(List()))))

        val response: JsonRpcResponse = jsonRpcController.handleRequest(getPendingTransactionsRpcRequest).futureValue

        response should haveResult(JArray(List()))
      }

      "mempool has transactions" in new TestSetup {
        val transactions = (0 to 1).map(_ => {
          val fakeTransaction = SignedTransactionWithSender(
            Transaction(
              nonce = 0,
              gasPrice = 123,
              gasLimit = 123,
              receivingAddress = Address("0x1234"),
              value = 0,
              payload = ByteString()
            ),
            signature = ECDSASignature(0, 0, 0.toByte),
            sender = Address("0x1234")
          )
          PendingTransaction(fakeTransaction, System.currentTimeMillis)
        })
        (qaService.getPendingTransactions _)
          .expects(getPendingTransactionReq)
          .returning(Future.successful(Right(GetPendingTransactionsResponse(transactions))))

        val response: JsonRpcResponse = jsonRpcController.handleRequest(getPendingTransactionsRpcRequest).futureValue

        val result = JArray(
          transactions
            .map(tx => {
              encodeAsHex(tx.stx.tx.hash)
            })
            .toList
        )

        response should haveResult(result)
      }
    }
  }

  trait TestSetup extends MockFactory with JRCMatchers {
    def config: JsonRpcConfig = JsonRpcConfig(Config.config)

    val appStateStorage = mock[AppStateStorage]
    val web3Service = mock[Web3Service]
    val netService = mock[NetService]
    val personalService = mock[PersonalService]
    val debugService = mock[DebugService]
    val ethService = mock[EthService]

    val qaService = mock[QAService]
    val jsonRpcController =
      new JsonRpcController(web3Service, netService, ethService, personalService, None, debugService, qaService, config)

    val mineBlocksReq = MineBlocksRequest(1, true, None)
    val getPendingTransactionReq = GetPendingTransactionsRequest()

    val mineBlocksRpcRequest = JsonRpcRequest(
      "2.0",
      "qa_mineBlocks",
      Some(
        JArray(
          List(
            JInt(1),
            JBool(true)
          )
        )
      ),
      Some(JInt(1))
    )

    val getPendingTransactionsRpcRequest = JsonRpcRequest(
      "2.0",
      "qa_getPendingTransactions",
      Some(
        JArray(
          List()
        )
      ),
      Some(JInt(1))
    )

    def msg(str: String): JField = "message" -> JString(str)
    val nullMessage: JField = "message" -> JNull

    def responseType(expectedType: MineBlocksResponse.MinerResponseType): JField =
      "responseType" -> JString(expectedType.entryName)

    def mockSuccessfulMineBlocksBehaviour(resp: MinerResponse) = {
      (qaService.mineBlocks _)
        .expects(mineBlocksReq)
        .returning(Future.successful(Right(MineBlocksResponse(resp))))
    }

    val fakeChainId: Byte = 42.toByte
  }
}
