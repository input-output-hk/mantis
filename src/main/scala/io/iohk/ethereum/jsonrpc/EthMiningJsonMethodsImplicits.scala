package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.EthMiningService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.{ JsonEncoder, JsonMethodDecoder }
import org.json4s.JsonAST
import org.json4s.JsonAST.{ JArray, JBool, JString, JValue }

object EthMiningJsonMethodsImplicits extends JsonMethodsImplicits  {
  implicit val eth_mining = new NoParamsMethodDecoder(GetMiningRequest()) with JsonEncoder[GetMiningResponse] {
    override def encodeJson(t: GetMiningResponse): JValue = JBool(t.isMining)
  }

  implicit val eth_getWork = new NoParamsMethodDecoder(GetWorkRequest()) with JsonEncoder[GetWorkResponse] {
    override def encodeJson(t: GetWorkResponse): JsonAST.JValue = {
      val powHeaderHash = encodeAsHex(t.powHeaderHash)
      val dagSeed = encodeAsHex(t.dagSeed)
      val target = encodeAsHex(t.target)
      JArray(List(powHeaderHash, dagSeed, target))
    }
  }

  implicit val eth_submitHashrate = new JsonMethodDecoder[SubmitHashRateRequest]
    with JsonEncoder[SubmitHashRateResponse] {
    override def decodeJson(params: Option[JsonAST.JArray]): Either[JsonRpcError, SubmitHashRateRequest] =
      params match {
        case Some(JArray(hashRate :: JString(id) :: Nil)) =>
          val result: Either[JsonRpcError, SubmitHashRateRequest] = for {
            rate <- extractQuantity(hashRate)
            miner <- extractHash(id)
          } yield SubmitHashRateRequest(rate, miner)
          result
        case _ =>
          Left(InvalidParams())
      }

    override def encodeJson(t: SubmitHashRateResponse): JValue = JBool(t.success)
  }

  implicit val eth_hashrate = new NoParamsMethodDecoder(GetHashRateRequest()) with JsonEncoder[GetHashRateResponse] {
    override def encodeJson(t: GetHashRateResponse): JsonAST.JValue = encodeAsHex(t.hashRate)
  }

  implicit val eth_coinbase = new NoParamsMethodDecoder(GetCoinbaseRequest()) with JsonEncoder[GetCoinbaseResponse] {
    override def encodeJson(t: GetCoinbaseResponse): JsonAST.JValue = {
      encodeAsHex(t.address.bytes)
    }
  }

  implicit val eth_submitWork = new JsonMethodDecoder[SubmitWorkRequest] with JsonEncoder[SubmitWorkResponse] {
    override def decodeJson(params: Option[JsonAST.JArray]): Either[JsonRpcError, SubmitWorkRequest] = params match {
      case Some(JArray(JString(nonce) :: JString(powHeaderHash) :: JString(mixHash) :: Nil)) =>
        for {
          n <- extractBytes(nonce)
          p <- extractBytes(powHeaderHash)
          m <- extractBytes(mixHash)
        } yield SubmitWorkRequest(n, p, m)
      case _ =>
        Left(InvalidParams())
    }

    override def encodeJson(t: SubmitWorkResponse): JValue = JBool(t.success)
  }

}
