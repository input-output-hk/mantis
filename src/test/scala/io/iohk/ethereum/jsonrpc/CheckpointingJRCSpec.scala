package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.checkpointing.CheckpointingTestHelpers
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.jsonrpc.CheckpointingService._
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.utils.{ByteStringUtils, Config}
import io.iohk.ethereum.{Fixtures, NormalPatience, crypto}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckpointingJRCSpec
    extends AnyFlatSpec
    with Matchers
    with MockFactory
    with ScalaFutures
    with NormalPatience
    with JRCMatchers
    with JsonMethodsImplicits
    with SecureRandomBuilder {

  import Req._

  "CheckpointingJRC" should "getLatestBlock" in new TestSetup {
    val request = getLatestBlockRequestBuilder(JArray(JInt(4) :: Nil))
    val servResp = GetLatestBlockResponse(block.hash, block.number)
    (checkpointingService.getLatestBlock _)
      .expects(GetLatestBlockRequest(4))
      .returning(Task.now(Right(servResp)))

    val expectedResult = JObject(
      "hash" -> JString("0x" + ByteStringUtils.hash2string(block.hash)),
      "number" -> JInt(block.number)
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveResult(expectedResult)
  }

  it should "return invalid params when checkpoint interval is not positive (getLatestBlock)" in new TestSetup {
    val request = getLatestBlockRequestBuilder(JArray(JInt(-1) :: Nil))

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(expectedPositiveIntegerError)
  }

  it should "return invalid params when checkpoint interval is too big (getLatestBlock)" in new TestSetup {
    val request = getLatestBlockRequestBuilder(JArray(JInt(BigInt(Int.MaxValue) + 1) :: Nil))

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(expectedPositiveIntegerError)
  }

  it should "return invalid params when checkpoint interval is missing (getLatestBlock)" in new TestSetup {
    val request = getLatestBlockRequestBuilder(JArray(Nil))

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(InvalidParams())
  }

  it should "pushCheckpoint" in new TestSetup {
    val request = pushCheckpointRequestBuilder(
      JArray(
        JString(ByteStringUtils.hash2string(block.hash))
          :: JArray(signatures.map(sig => JString(ByteStringUtils.hash2string(sig.toBytes))))
          :: Nil
      )
    )
    val servResp = PushCheckpointResponse()
    val servReq = PushCheckpointRequest(
      block.hash,
      signatures
    )

    (checkpointingService.pushCheckpoint _)
      .expects(servReq)
      .returning(Task.now(Right(servResp)))

    val expectedResult = JBool(true)

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveResult(expectedResult)
  }

  it should "return invalid params when some arguments are missing (pushCheckpoint)" in new TestSetup {
    val request = pushCheckpointRequestBuilder(
      JArray(JString(ByteStringUtils.hash2string(block.hash)) :: Nil)
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(InvalidParams())
  }

  it should "return invalid params when hash has bad length (pushCheckpoint)" in new TestSetup {
    val badHash = ByteStringUtils.hash2string(block.hash).dropRight(2)
    val request = pushCheckpointRequestBuilder(
      JArray(
        JString(badHash)
          :: JArray(signatures.map(sig => JString(ByteStringUtils.hash2string(sig.toBytes))))
          :: Nil
      )
    )

    val expectedError = InvalidParams(s"Invalid value [$badHash], expected 32 bytes")

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(expectedError)
  }

  it should "return invalid params when hash has bad format (pushCheckpoint)" in new TestSetup {
    val badHash = ByteStringUtils.hash2string(block.hash).replaceAll("0", "X")
    val request = pushCheckpointRequestBuilder(
      JArray(
        JString(badHash)
          :: JArray(signatures.map(sig => JString(ByteStringUtils.hash2string(sig.toBytes))))
          :: Nil
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(InvalidParams())
  }

  it should "return invalid params when signatures are not strings (pushCheckpoint)" in new TestSetup {
    val request = pushCheckpointRequestBuilder(
      JArray(
        JString(ByteStringUtils.hash2string(block.hash))
          :: JArray(signatures.map(sig => JBool(true)))
          :: Nil
      )
    )

    val expectedError = InvalidParams("Unable to extract a signature from: JBool(true)")

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(expectedError)
  }

  it should "return invalid params when signatures have bad format (pushCheckpoint)" in new TestSetup {
    val request = pushCheckpointRequestBuilder(
      JArray(
        JString(ByteStringUtils.hash2string(block.hash))
          :: JArray(signatures.map(sig => JString(ByteStringUtils.hash2string(sig.toBytes).replaceAll("0", "X"))))
          :: Nil
      )
    )

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(InvalidParams())
  }

  it should "return invalid params when signatures have bad length (pushCheckpoint)" in new TestSetup {
    val request = pushCheckpointRequestBuilder(
      JArray(
        JString(ByteStringUtils.hash2string(block.hash))
          :: JArray(signatures.map(sig => JString(ByteStringUtils.hash2string(sig.toBytes).dropRight(2))))
          :: Nil
      )
    )

    val expectedError = InvalidParams("Bad signature length")

    val response = jsonRpcController.handleRequest(request).runSyncUnsafe()
    response should haveError(expectedError)
  }

  object Req {
    val block = Fixtures.Blocks.ValidBlock.block

    val keys = Seq(
      crypto.generateKeyPair(secureRandom),
      crypto.generateKeyPair(secureRandom)
    )

    val signatures: List[ECDSASignature] = CheckpointingTestHelpers.createCheckpointSignatures(keys, block.hash).toList

    def getLatestBlockRequestBuilder(json: JArray) = JsonRpcRequest(
      "2.0",
      "checkpointing_getLatestBlock",
      Some(json),
      Some(1)
    )

    val expectedPositiveIntegerError = InvalidParams("Expected positive integer")

    def pushCheckpointRequestBuilder(json: JArray) = JsonRpcRequest(
      "2.0",
      "checkpointing_pushCheckpoint",
      Some(json),
      Some(1)
    )
  }

  trait TestSetup extends ApisBuilder {
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
        ProofServiceDummy,
        config
      )

  }
}
