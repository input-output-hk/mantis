package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.TestService._
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodDecoder}
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.bouncycastle.util.encoders.Hex
import cats.implicits._
import io.iohk.ethereum.blockchain.data.GenesisAccount

import scala.util.Try
import io.iohk.ethereum.domain.UInt256

object TestJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val test_setChainParams = new JsonMethodDecoder[SetChainParamsRequest]
    with JsonEncoder[SetChainParamsResponse] {

    private def extractAccount(accountJson: JValue): Either[JsonRpcError, GenesisAccount] =
      for {
        storageObject <- Try((accountJson \ "storage").extract[JObject]).toEither.left.map(e =>
          InvalidParams(e.toString())
        )
        storage <- storageObject.obj.traverse {
          case (key, JString(value)) =>
            Try(UInt256(decode(key)) -> UInt256(decode(value))).toEither.left.map(e => InvalidParams(e.toString()))
          case _ => Left(InvalidParams())
        }
        balance = UInt256(decode((accountJson \ "balance").extract[String]))
        code = ByteString(decode((accountJson \ "code").extract[String]))
        nonce = UInt256(decode((accountJson \ "nonce").extract[String]))
      } yield GenesisAccount(
        None,
        balance,
        code,
        nonce,
        storage.toMap
      )

    private def extractAccounts(accountsJson: JValue): Either[JsonRpcError, Map[ByteString, GenesisAccount]] =
      for {
        mapping <- Try(accountsJson.extract[JObject]).toEither.left.map(e => InvalidParams(e.toString()))
        accounts <- mapping.obj.traverse { case (key, value) =>
          for {
            address <- extractBytes(key)
            account <- extractAccount(value)
          } yield address -> account
        }
      } yield accounts.toMap

    private def extractBlockchainParams(blockchainParamsJson: JValue): Either[JsonRpcError, BlockchainParams] = {
      for {
        eIP150ForkBlock <- extractQuantity(blockchainParamsJson \ "EIP150ForkBlock")
        eIP158ForkBlock <- extractQuantity(blockchainParamsJson \ "EIP158ForkBlock")
        accountStartNonce <- optionalQuantity(blockchainParamsJson \ "accountStartNonce")
        allowFutureBlocks = (blockchainParamsJson \ "allowFutureBlocks").extractOrElse(true)
        blockReward <- optionalQuantity(blockchainParamsJson \ "blockReward")
        byzantiumForkBlock <- extractQuantity(blockchainParamsJson \ "byzantiumForkBlock")
        homesteadForkBlock <- extractQuantity(blockchainParamsJson \ "homesteadForkBlock")
      } yield BlockchainParams(
        eIP150ForkBlock,
        eIP158ForkBlock,
        accountStartNonce.getOrElse(0),
        allowFutureBlocks,
        blockReward.getOrElse(0),
        byzantiumForkBlock,
        homesteadForkBlock,
        0
      )
    }

    private def extractGenesis(genesisJson: JValue): Either[JsonRpcError, GenesisParams] = {
      for {
        author <- extractBytes((genesisJson \ "author").extract[String])
        difficulty = (genesisJson \ "difficulty").extractOrElse("0")
        extraData <- extractBytes((genesisJson \ "extraData").extract[String])
        gasLimit <- extractQuantity(genesisJson \ "gasLimit")
        parentHash <- extractBytes((genesisJson \ "parentHash").extractOrElse(""))
        timestamp <- extractBytes((genesisJson \ "timestamp").extract[String])
        nonce <- extractBytes((genesisJson \ "nonce").extract[String])
        mixHash <- extractBytes((genesisJson \ "mixHash").extract[String])
      } yield GenesisParams(author, difficulty, extraData, gasLimit, parentHash, timestamp, nonce, mixHash)
    }

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

  implicit val test_importRawBlock = new JsonMethodDecoder[ImportRawBlockRequest]
    with JsonEncoder[ImportRawBlockResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, ImportRawBlockRequest] =
      params match {
        case Some(JArray(JString(blockRlp) :: Nil)) =>
          Right(ImportRawBlockRequest(blockRlp))
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: ImportRawBlockResponse): JValue = t.blockHash
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
