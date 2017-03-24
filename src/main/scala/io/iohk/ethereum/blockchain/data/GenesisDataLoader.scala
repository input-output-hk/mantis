package io.iohk.ethereum.blockchain.data

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.messages.PV63.AccountImplicits
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.utils.{Config, Logger}
import io.iohk.ethereum.{rlp, crypto}
import io.iohk.ethereum.db.dataSource.{EphemDataSource, DataSource}
import io.iohk.ethereum.db.storage.{Namespaces, NodeStorage}
import io.iohk.ethereum.domain.{Block, BlockHeader, Account, Blockchain}
import io.iohk.ethereum.mpt.{RLPByteArraySerializable, MerklePatriciaTrie}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.rlp.RLPImplicits._
import org.spongycastle.util.encoders.Hex
import spray.json._

import scala.io.Source
import scala.util.{Failure, Success, Try}

class GenesisDataLoader(dataSource: DataSource, blockchain: Blockchain) extends Logger{

  import GenesisDataLoader._
  import JsonProtocol._
  import AccountImplicits._

  private val bloomLength = 512
  private val hashLength = 64
  private val addressLength = 40

  private implicit val accountSerializer = new RLPByteArraySerializable[Account]

  private val emptyTrieRootHash = ByteString(crypto.kec256(rlp.encode(Array.emptyByteArray)))
  private val emptyEvmHash: ByteString = crypto.kec256(ByteString.empty)

  def loadGenesisData(): Unit = {
    import Config.Blockchain.customGenesisFileOpt

    log.info("Loading genesis data")

    val genesisJson = customGenesisFileOpt match {
      case Some(customGenesisFile) =>
        log.debug(s"Trying to load custom genesis data from file: $customGenesisFile")

        Try(Source.fromFile(customGenesisFile)) match {
          case Success(customGenesis) =>
            log.info(s"Using custom genesis data from file: $customGenesisFile")
            try {
              customGenesis.getLines().mkString
            } finally {
              customGenesis.close()
            }
          case Failure(ex) =>
            log.error(s"Cannot load custom genesis data from file: $customGenesisFile", ex)
            throw ex
        }
      case None =>
        val src = Source.fromResource("blockchain/default-genesis.json")
        try {
          src.getLines().mkString
        } finally {
          src.close()
        }
    }

    loadGenesisData(genesisJson) match {
      case Success(_) =>
        log.info("Genesis data successfully loaded")
      case Failure(ex) =>
        log.error("Unable to load genesis data", ex)
        throw ex
    }
  }

  private def loadGenesisData(genesisJson: String): Try[Unit] = {
    for {
      genesisData <- Try(genesisJson.parseJson.convertTo[GenesisData](genesisDataFormat))
      _ <- loadGenesisData(genesisData)
    } yield ()
  }

  private def loadGenesisData(genesisData: GenesisData): Try[Unit] = {
    import MerklePatriciaTrie.defaultByteArraySerializable

    val ephemDataSource = EphemDataSource()
    val ephemNodeStorage = new NodeStorage(ephemDataSource)

    val initialStateMpt =
      MerklePatriciaTrie[Array[Byte], Account](ephemNodeStorage, (input: Array[Byte]) => crypto.kec256(input))
    val stateMpt = genesisData.alloc.foldLeft(initialStateMpt) { case (mpt, (address, AllocAccount(balance))) =>
      val paddedAddress = address.reverse.padTo(addressLength, "0").reverse.mkString
      mpt.put(crypto.kec256(Hex.decode(paddedAddress)), Account(0, BigInt(balance), emptyTrieRootHash, emptyEvmHash))
    }

    val header = BlockHeader(
      parentHash = zeros(hashLength),
      ommersHash = ByteString(crypto.kec256(rlp.encode(RLPList()))),
      beneficiary = genesisData.coinbase,
      stateRoot = ByteString(stateMpt.getRootHash),
      transactionsRoot = emptyTrieRootHash,
      receiptsRoot = emptyTrieRootHash,
      logsBloom = zeros(bloomLength),
      difficulty = BigInt(genesisData.difficulty.replace("0x", ""), 16),
      number = 0,
      gasLimit = BigInt(genesisData.gasLimit.replace("0x", ""), 16),
      gasUsed = 0,
      unixTimestamp = BigInt(genesisData.timestamp.replace("0x", ""), 16).toLong,
      extraData = genesisData.extraData,
      mixHash = genesisData.mixHash.getOrElse(zeros(hashLength)),
      nonce = genesisData.nonce)

    blockchain.getBlockHeaderByNumber(0) match {
      case Some(existingGenesisHeader) if existingGenesisHeader.hash == header.hash =>
        log.info("Genesis data already in the database")
        Success(())

      case Some(_) =>
        Failure(new RuntimeException("Genesis data present in the database does not match genesis block from file." +
          " Use different directory for running private blockchains."))

      case None =>
        dataSource.update(Namespaces.NodeNamespace, Nil, ephemDataSource.storage.toSeq)
        blockchain.save(Block(header, BlockBody(Nil, Nil)))
        blockchain.save(header.hash, Nil)
        blockchain.save(header.hash, header.difficulty)
        Success(())
    }
  }

  private def zeros(length: Int) =
    ByteString(Hex.decode(List.fill(length)("0").mkString))

}

object GenesisDataLoader {
  implicit object JsonProtocol extends DefaultJsonProtocol {

    implicit object ByteStringJsonFormat extends RootJsonFormat[ByteString] {
      def read(value: JsValue): ByteString = value match {
        case s: JsString =>
          val noPrefix = s.value.replace("0x", "")
          val inp =
            if (noPrefix.length % 2 == 0) noPrefix
            else "0" ++ noPrefix
          Try(ByteString(Hex.decode(inp))) match {
            case Success(bs) => bs
            case Failure(ex) => deserializationError("Cannot parse hex string: " + s)
          }
        case other => deserializationError("Expected hex string, but got: " + other)
      }

      override def write(obj: ByteString): JsValue =
        throw new RuntimeException("ByteStringJsonFormat.write should never be called")
    }

    implicit val allocAccountFormat = jsonFormat1(AllocAccount)
    implicit val genesisDataFormat = jsonFormat8(GenesisData)
  }
}
