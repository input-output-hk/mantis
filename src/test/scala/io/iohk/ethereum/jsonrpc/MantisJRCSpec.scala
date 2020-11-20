package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.MantisService.GetAccountTransactionsResponse
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.transactions.TransactionHistoryService.ExtendedTransactionData
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.{Fixtures, FreeSpecBase, SpecFixtures}
import monix.eval.Task
import org.json4s.{Extraction, JArray, JBool, JInt, JObject, JString}
import org.scalamock.scalatest.AsyncMockFactory

class MantisJRCSpec extends FreeSpecBase with SpecFixtures with AsyncMockFactory with JRCMatchers {
  import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers.formats

  class Fixture extends ApisBuilder {
    def config: JsonRpcConfig = JsonRpcConfig(Config.config, available)

    val web3Service = mock[Web3Service]
    val netService = mock[NetService]
    val personalService = mock[PersonalService]
    val debugService = mock[DebugService]
    val ethService = mock[EthService]
    val qaService = mock[QAService]
    val checkpointingService = mock[CheckpointingService]
    val mantisService = mock[MantisService]

    val jsonRpcController =
      new JsonRpcController(
        web3Service,
        netService,
        ethService,
        personalService,
        None,
        debugService,
        qaService,
        checkpointingService,
        mantisService,
        config
      )

  }
  def createFixture() = new Fixture

  "Mantis JRC" - {
    "should handle mantis_getAccountTransactions" in testCaseM { fixture =>
      import fixture._
      val block = Fixtures.Blocks.Block3125369
      val sentTx = block.body.transactionList.head
      val receivedTx = block.body.transactionList.last

      (mantisService.getAccountTransactions _)
        .expects(*)
        .returning(
          Task.now(
            Right(
              GetAccountTransactionsResponse(
                List(
                  ExtendedTransactionData(sentTx, isOutgoing = true, Some((block.header, 0))),
                  ExtendedTransactionData(receivedTx, isOutgoing = false, Some((block.header, 1)))
                )
              )
            )
          )
        )

      val request: JsonRpcRequest = JsonRpcRequest(
        "2.0",
        "mantis_getAccountTransactions",
        Some(
          JArray(
            List(
              JString(s"0x7B9Bc474667Db2fFE5b08d000F1Acc285B2Ae47D"),
              JInt(100),
              JInt(200)
            )
          )
        ),
        Some(JInt(1))
      )

      val expectedTxs = Seq(
        JObject(
          Extraction
            .decompose(TransactionResponse(sentTx, Some(block.header), Some(0)))
            .asInstanceOf[JObject]
            .obj ++ List(
            "isPending" -> JBool(false),
            "isOutgoing" -> JBool(true)
          )
        ),
        JObject(
          Extraction
            .decompose(TransactionResponse(receivedTx, Some(block.header), Some(1)))
            .asInstanceOf[JObject]
            .obj ++ List(
            "isPending" -> JBool(false),
            "isOutgoing" -> JBool(false)
          )
        )
      )

      for {
        response <- jsonRpcController.handleRequest(request)
      } yield response should haveObjectResult("transactions" -> JArray(expectedTxs.toList))
    }
  }
}
