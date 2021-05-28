package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.TestService._
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodDecoder}
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import cats.implicits._
import io.iohk.ethereum.blockchain.data.GenesisAccount

import scala.util.Try
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.testmode.SealEngineType
import org.json4s.Extraction

object TestJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val test_setChainParams: JsonMethodDecoder[SetChainParamsRequest] with JsonEncoder[SetChainParamsResponse] =
    new JsonMethodDecoder[SetChainParamsRequest] with JsonEncoder[SetChainParamsResponse] {

      private def extractAccounts(accountsJson: JValue): Either[JsonRpcError, Map[ByteString, GenesisAccount]] =
        for {
          mapping <- Try(accountsJson.extract[JObject]).toEither.leftMap(e => InvalidParams(e.toString))
          accounts <- mapping.obj.traverse { case (key, value) =>
            for {
              address <- extractBytes(key)
              account <- extractAccount(value)
            } yield address -> account
          }
        } yield accounts.toMap

      private def extractAccount(accountJson: JValue): Either[JsonRpcError, GenesisAccount] =
        for {
          storageObject <- Try((accountJson \ "storage").extract[JObject]).toEither.leftMap(e =>
            InvalidParams(e.toString)
          )
          storage <- storageObject.obj.traverse {
            case (key, JString(value)) =>
              Try(UInt256(decode(key)) -> UInt256(decode(value))).toEither.leftMap(e => InvalidParams(e.toString))
            case _ => Left(InvalidParams())
          }
          balance = UInt256(decode((accountJson \ "balance").extract[String]))
          code = decode((accountJson \ "code").extract[String])
          codeOpt = if (code.isEmpty) None else Some(ByteString(code))
          nonce = decode((accountJson \ "nonce").extract[String])
          nonceOpt = if (nonce.isEmpty || UInt256(nonce) == UInt256.Zero) None else Some(UInt256(nonce))
        } yield GenesisAccount(
          None,
          balance,
          codeOpt,
          nonceOpt,
          Some(storage.toMap)
        )

      def decodeJson(params: Option[JArray]): Either[JsonRpcError, SetChainParamsRequest] =
        params match {
          case Some(JArray(paramsObj :: Nil)) =>
            for {
              genesis <- extractGenesis(paramsObj \ "genesis")
              blockchainParams <- extractBlockchainParams(paramsObj \ "params")
              sealEngine <- Try((paramsObj \ "sealEngine").extract[String]).toEither
                .leftMap(_ => InvalidParams())
                .flatMap(extractSealEngine)
              accounts <- extractAccounts(paramsObj \ "accounts")
            } yield SetChainParamsRequest(ChainParams(genesis, blockchainParams, sealEngine, accounts))
          case _ => Left(InvalidParams())
        }

      private def extractSealEngine(str: String) = str match {
        case "NoReward" => Right(SealEngineType.NoReward)
        case "NoProof" => Right(SealEngineType.NoProof)
        case other => Left(InvalidParams(s"unknown seal engine $other"))
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

      private def extractBlockchainParams(blockchainParamsJson: JValue): Either[JsonRpcError, BlockchainParams] = {
        for {
          eIP150ForkBlock <- optionalQuantity(blockchainParamsJson \ "EIP150ForkBlock")
          eIP158ForkBlock <- optionalQuantity(blockchainParamsJson \ "EIP158ForkBlock")
          accountStartNonce <- optionalQuantity(blockchainParamsJson \ "accountStartNonce")
          allowFutureBlocks = (blockchainParamsJson \ "allowFutureBlocks").extractOrElse(true)
          blockReward <- optionalQuantity(blockchainParamsJson \ "blockReward")
          byzantiumForkBlock <- optionalQuantity(blockchainParamsJson \ "byzantiumForkBlock")
          homesteadForkBlock <- optionalQuantity(blockchainParamsJson \ "homesteadForkBlock")
          constantinopleForkBlock <- optionalQuantity(blockchainParamsJson \ "constantinopleForkBlock")
          istanbulForkBlock <- optionalQuantity(blockchainParamsJson \ "istanbulForkBlock")
        } yield BlockchainParams(
          EIP150ForkBlock = eIP150ForkBlock,
          EIP158ForkBlock = eIP158ForkBlock,
          accountStartNonce = accountStartNonce.getOrElse(0),
          allowFutureBlocks = allowFutureBlocks,
          blockReward = blockReward.getOrElse(0),
          byzantiumForkBlock = byzantiumForkBlock,
          homesteadForkBlock = homesteadForkBlock,
          maximumExtraDataSize = 0,
          constantinopleForkBlock = constantinopleForkBlock,
          istanbulForkBlock = istanbulForkBlock
        )
      }

      override def encodeJson(t: SetChainParamsResponse): JValue = true
    }

  implicit val test_mineBlocks: JsonMethodDecoder[MineBlocksRequest] with JsonEncoder[MineBlocksResponse] =
    new JsonMethodDecoder[MineBlocksRequest] with JsonEncoder[MineBlocksResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, MineBlocksRequest] =
        params match {
          case Some(JArray(JInt(numBlocks) :: Nil)) =>
            Right(MineBlocksRequest(numBlocks.toInt))
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: MineBlocksResponse): JValue = true
    }

  implicit val test_modifyTimestamp
      : JsonMethodDecoder[ModifyTimestampRequest] with JsonEncoder[ModifyTimestampResponse] =
    new JsonMethodDecoder[ModifyTimestampRequest] with JsonEncoder[ModifyTimestampResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, ModifyTimestampRequest] =
        params match {
          case Some(JArray(JInt(timestamp) :: Nil)) =>
            Right(ModifyTimestampRequest(timestamp.toLong))
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: ModifyTimestampResponse): JValue = true
    }

  implicit val test_rewindToBlock: JsonMethodDecoder[RewindToBlockRequest] with JsonEncoder[RewindToBlockResponse] =
    new JsonMethodDecoder[RewindToBlockRequest] with JsonEncoder[RewindToBlockResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, RewindToBlockRequest] =
        params match {
          case Some(JArray(JInt(blockNum) :: Nil)) =>
            Right(RewindToBlockRequest(blockNum.toLong))
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: RewindToBlockResponse): JValue = true
    }

  implicit val test_importRawBlock: JsonMethodDecoder[ImportRawBlockRequest] with JsonEncoder[ImportRawBlockResponse] =
    new JsonMethodDecoder[ImportRawBlockRequest] with JsonEncoder[ImportRawBlockResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, ImportRawBlockRequest] =
        params match {
          case Some(JArray(JString(blockRlp) :: Nil)) =>
            Right(ImportRawBlockRequest(blockRlp))
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: ImportRawBlockResponse): JValue = t.blockHash
    }

  implicit val miner_setEtherbase: JsonMethodDecoder[SetEtherbaseRequest] with JsonEncoder[SetEtherbaseResponse] =
    new JsonMethodDecoder[SetEtherbaseRequest] with JsonEncoder[SetEtherbaseResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, SetEtherbaseRequest] =
        params match {
          case Some(JArray((addressStr: JString) :: Nil)) =>
            extractAddress(addressStr).map(address => SetEtherbaseRequest(address))
          case _ => Left(InvalidParams())
        }

      def encodeJson(t: SetEtherbaseResponse): JValue = true
    }

  implicit val debug_accountRange: JsonMethodDecoder[AccountsInRangeRequest] with JsonEncoder[AccountsInRangeResponse] =
    new JsonMethodDecoder[AccountsInRangeRequest] with JsonEncoder[AccountsInRangeResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, AccountsInRangeRequest] =
        params match {
          case Some(JArray(blockHashOrNumber :: txIndex :: addressHash :: maxResults :: Nil)) =>
            for {
              txIndex <- extractQuantity(txIndex)
              maxResults <- extractQuantity(maxResults)
              addressHash <- extractBytes(addressHash.extract[String])
              blockHashOrNumberEither = extractBlockHashOrNumber(blockHashOrNumber.extract[String])
            } yield AccountsInRangeRequest(
              AccountsInRangeRequestParams(blockHashOrNumberEither, txIndex, addressHash, maxResults)
            )
          case _ => Left(InvalidParams())
        }

      private def extractBlockHashOrNumber(blockHash: String): Either[BigInt, ByteString] =
        extractHash(blockHash).fold(_ => Left(BigInt(blockHash)), Right(_))

      override def encodeJson(t: AccountsInRangeResponse): JValue = JObject(
        "addressMap" -> JObject(
          t.addressMap.toList.map(addressPair => encodeAsHex(addressPair._1).values -> encodeAsHex(addressPair._2))
        ),
        "nextKey" -> encodeAsHex(t.nextKey)
      )
    }

  implicit val debug_storageRangeAt: JsonMethodDecoder[StorageRangeRequest] with JsonEncoder[StorageRangeResponse] =
    new JsonMethodDecoder[StorageRangeRequest] with JsonEncoder[StorageRangeResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, StorageRangeRequest] =
        params match {
          case Some(JArray(blockHashOrNumber :: txIndex :: address :: begin :: maxResults :: Nil)) =>
            for {
              txIndex <- extractQuantity(txIndex)
              maxResults <- extractQuantity(maxResults)
              begin <- extractQuantity(begin)
              addressHash <- extractBytes(address.extract[String])
              blockHashOrNumberEither = extractBlockHashOrNumber(blockHashOrNumber.extract[String])
            } yield StorageRangeRequest(
              StorageRangeParams(blockHashOrNumberEither, txIndex, addressHash, begin, maxResults)
            )
          case _ => Left(InvalidParams())
        }

      private def extractBlockHashOrNumber(blockHash: String): Either[BigInt, ByteString] =
        extractHash(blockHash).fold(_ => Left(BigInt(blockHash)), Right(_))

      override def encodeJson(t: StorageRangeResponse): JValue = Extraction.decompose(t)
    }

  implicit val test_getLogHash: JsonMethodDecoder[GetLogHashRequest] with JsonEncoder[GetLogHashResponse] =
    new JsonMethodDecoder[GetLogHashRequest] with JsonEncoder[GetLogHashResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetLogHashRequest] =
        params match {
          case Some(JArray(JString(transactionHashString) :: Nil)) =>
            extractHash(transactionHashString).map(th => GetLogHashRequest(th))
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: GetLogHashResponse): JValue = encodeAsHex(t.logHash)
    }
}
