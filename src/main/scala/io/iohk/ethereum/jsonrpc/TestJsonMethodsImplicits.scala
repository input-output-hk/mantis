package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.TestService._
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodDecoder}
import org.bouncycastle.util.encoders.Hex
import org.json4s.JsonAST._
import org.json4s.JsonDSL._

import scala.util.Try

object TestJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val test_setChainParams = new JsonMethodDecoder[SetChainParamsRequest]
    with JsonEncoder[SetChainParamsResponse] {
    private def extractAccounts(accountsJson: JValue): Either[JsonRpcError, Map[ByteString, AccountConfig]] = {
      val accounts = accountsJson.asInstanceOf[JObject].values.collect { case (key, accObj: Map[_, _]) =>
        val rawWei = accObj.asInstanceOf[Map[String, Any]]("wei").asInstanceOf[String]
        val wei =
          if (rawWei.startsWith("0x")) BigInt(rawWei.replace("0x", ""), 16)
          else BigInt(rawWei, 10)
        ByteString(Hex.decode(key.replace("0x", ""))) -> AccountConfig(None, wei)
      }
      Right(accounts)
    }

    private def extractBlockchainParams(blockchainParamsJson: JValue): Either[JsonRpcError, BlockchainParams] =
      for {
        eIP150ForkBlock <- extractQuantity(blockchainParamsJson \ "EIP150ForkBlock")
        eIP158ForkBlock <- extractQuantity(blockchainParamsJson \ "EIP158ForkBlock")
        accountStartNonce <- extractQuantity(blockchainParamsJson \ "accountStartNonce")
        allowFutureBlocks <- Try((blockchainParamsJson \ "allowFutureBlocks").extract[Boolean]).toEither.left.map(_ =>
          InvalidParams()
        )
        blockReward <- extractQuantity(blockchainParamsJson \ "blockReward")
        byzantiumForkBlock <- extractQuantity(blockchainParamsJson \ "byzantiumForkBlock")
        homesteadForkBlock <- extractQuantity(blockchainParamsJson \ "homesteadForkBlock")
        maximumExtraDataSize <- extractQuantity(blockchainParamsJson \ "maximumExtraDataSize")
      } yield BlockchainParams(
        eIP150ForkBlock,
        eIP158ForkBlock,
        accountStartNonce,
        allowFutureBlocks,
        blockReward,
        byzantiumForkBlock,
        homesteadForkBlock,
        maximumExtraDataSize
      )

    private def extractGenesis(genesisJson: JValue): Either[JsonRpcError, GenesisParams] =
      for {
        author <- extractBytes((genesisJson \ "author").extract[String])
        extraData <- extractBytes((genesisJson \ "extraData").extract[String])
        gasLimit <- extractQuantity(genesisJson \ "gasLimit")
        parentHash <- extractBytes((genesisJson \ "parentHash").extract[String])
        timestamp <- extractBytes((genesisJson \ "timestamp").extract[String])
      } yield GenesisParams(author, extraData, gasLimit, parentHash, timestamp)

    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SetChainParamsRequest] =
      params match {
        case Some(JArray(paramsObj :: Nil)) =>
          for {
            genesis <- extractGenesis(paramsObj \ "genesis")
            blockchainParams <- extractBlockchainParams(paramsObj \ "params")
            sealEngine <- Try((paramsObj \ "sealEngine").extract[String]).toEither.left.map(_ => InvalidParams())
            accounts <- extractAccounts(paramsObj \ "accounts")
          } yield SetChainParamsRequest(ChainParams(genesis, blockchainParams, sealEngine, accounts))
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: SetChainParamsResponse): JValue = true
  }

  implicit val test_mineBlocks = new JsonMethodDecoder[MineBlocksRequest] with JsonEncoder[MineBlocksResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, MineBlocksRequest] =
      params match {
        case Some(JArray(JInt(numBlocks) :: Nil)) =>
          Right(MineBlocksRequest(numBlocks.toInt))
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: MineBlocksResponse): JValue = true
  }

  implicit val test_modifyTimestamp = new JsonMethodDecoder[ModifyTimestampRequest]
    with JsonEncoder[ModifyTimestampResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, ModifyTimestampRequest] =
      params match {
        case Some(JArray(JInt(timestamp) :: Nil)) =>
          Right(ModifyTimestampRequest(timestamp.toLong))
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: ModifyTimestampResponse): JValue = true
  }

  implicit val test_rewindToBlock = new JsonMethodDecoder[RewindToBlockRequest]
    with JsonEncoder[RewindToBlockResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, RewindToBlockRequest] =
      params match {
        case Some(JArray(JInt(blockNum) :: Nil)) =>
          Right(RewindToBlockRequest(blockNum.toLong))
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: RewindToBlockResponse): JValue = true
  }

  implicit val miner_setEtherbase = new JsonMethodDecoder[SetEtherbaseRequest] with JsonEncoder[SetEtherbaseResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SetEtherbaseRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
          } yield SetEtherbaseRequest(address)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: SetEtherbaseResponse): JValue = true
  }
}
