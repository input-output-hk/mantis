package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.domain.{Address, BlockchainImpl}
import io.iohk.ethereum.jsonrpc.ProofService.{ProveAccountRequest, ProveAccountResponse}
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.mpt.MerklePatriciaTrie.ProofSketch
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import org.json4s.JsonAST.{JArray, JObject, JString, JValue}

import scala.concurrent.Future
import scala.util.{Failure, Success}

// FIXME Use a dedicated context
import scala.concurrent.ExecutionContext.Implicits.global

object ProofService {

  /** Prove account existence (request) */
  final case class ProveAccountRequest(address: Address)

  /** Prove account existence (response) */
  final case class ProveAccountResponse(proofOpt: Option[ProofSketch])

  object Method {
    final val ProveAccount = "proof_proveAccount"
  }
}

object ProofServiceJson extends JsonMethodsImplicits {
  import JsonRpcErrors._

  implicit val proof_proveAccount = new Codec[ProveAccountRequest, ProveAccountResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, ProveAccountRequest] = {
      params match {
        case Some(JArray(JString(addr) :: Nil)) ⇒
          for {
            address ← extractAddress(addr)
          } yield ProveAccountRequest(address)

        case  _ ⇒
          Left(InvalidRequest)
      }
    }

    def encodeJson(t: ProveAccountResponse): JValue = {
      t.proofOpt match {
        case None ⇒
          JObject(Nil)

        case Some(proof) ⇒
          val steps = proof.steps
          val jsonSteps = JArray(
            steps.map(step ⇒ JObject(
              ("nibbles", encodeAsHex(step.nibblesToByteString)),
              ("hash", encodeAsHex(step.hashToByteString))
            ))
          )

          val jObject = JObject(("steps", jsonSteps))

          jObject
      }
    }
  }
}

/**
 * A service that provides proofs about data stored in an MPT.
 */
class ProofService(
  blockchainConfig: BlockchainConfig,
  private[jsonrpc] val blockchain: BlockchainImpl // also visible to tests
) extends Logger {
  def proveAccount(req: ProveAccountRequest): ServiceResponse[ProveAccountResponse] = {
    Future {
      val address = req.address

      // Get an Accounts MPT for the blockchain
      val mpt = MerklePatriciaTrie.ofAccounts(blockchain)
      val proofOpt = mpt.prove(address)
      val response = ProveAccountResponse(proofOpt)

      response
    }.transformWith {
      case Success(proveAccountResponse) ⇒
        Future.successful(Right(proveAccountResponse))

      case Failure(exception) ⇒
        log.error(s"proveAccount($req) -> $exception")
        Future.successful(Left(JsonRpcErrors.InternalError))
    }
  }
}
