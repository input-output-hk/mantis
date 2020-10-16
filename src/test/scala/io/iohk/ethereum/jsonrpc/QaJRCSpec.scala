package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.consensus.ethash.MockedMinerProtocol.MineBlocks
import io.iohk.ethereum.consensus.ethash.{MinerResponse, MinerResponses}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.QAService.MineBlocksResponse.MinerResponseType._
import io.iohk.ethereum.jsonrpc.QAService.{
  MineBlocksRequest,
  MineBlocksResponse
}
import io.iohk.ethereum.utils.Config
import org.json4s.JsonAST._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import scala.concurrent.Future
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class QaJRCSpec extends AnyWordSpec with Matchers with ScalaFutures with NormalPatience with JsonMethodsImplicits {

  "QaJRC" should {
    "request block mining and return valid response with correct message" when {
      "mining ordered" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MinerResponses.MiningOrdered)

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveObjectResult(responseType(MiningOrdered), nullMessage)
      }

      "miner is working" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MinerResponses.MinerIsWorking)

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveObjectResult(responseType(MinerIsWorking), nullMessage)
      }

      "miner doesn't exist" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MinerResponses.MinerNotExist)

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveObjectResult(responseType(MinerNotExist), nullMessage)
      }

      "miner not support current msg" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MinerResponses.MinerNotSupport(MineBlocks(1, true)))

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveObjectResult(responseType(MinerNotSupport), msg("MineBlocks(1,true,None)"))
      }

      "miner return error" in new TestSetup {
        mockSuccessfulMineBlocksBehaviour(MinerResponses.MiningError("error"))

        val response: JsonRpcResponse = jsonRpcController.handleRequest(mineBlocksRpcRequest).futureValue

        response should haveObjectResult(responseType(MiningError), msg("error"))
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
  }

  trait TestSetup extends MockFactory with JRCMatchers {
    def config: JsonRpcConfig = JsonRpcConfig(Config.config)

    val appStateStorage = mock[AppStateStorage]
    val web3Service = mock[Web3Service]
    val netService = mock[NetService]
    val personalService = mock[PersonalService]
    val debugService = mock[DebugService]
    val ethService = mock[EthService]
    val checkpointingService = mock[CheckpointingService]

    val qaService = mock[QAService]
    val jsonRpcController =
      new JsonRpcController(web3Service, netService, ethService, personalService, None, debugService, qaService, checkpointingService, config)

    val mineBlocksReq = MineBlocksRequest(1, true, None)

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
