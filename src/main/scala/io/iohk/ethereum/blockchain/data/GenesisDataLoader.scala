package io.iohk.ethereum.blockchain.data

import java.io.FileNotFoundException

import akka.util.ByteString

import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.bouncycastle.util.encoders.Hex
import org.json4s.CustomSerializer
import org.json4s.DefaultFormats
import org.json4s.Formats
import org.json4s.JString
import org.json4s.JValue

import io.iohk.ethereum.blockchain.data.GenesisDataLoader.JsonSerializers.ByteStringJsonSerializer
import io.iohk.ethereum.blockchain.data.GenesisDataLoader.JsonSerializers.UInt256JsonSerializer
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.ArchiveNodeStorage
import io.iohk.ethereum.db.storage.MptStorage
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.db.storage.SerializingMptStorage
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.db.storage.StateStorage.GenesisDataLoad
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.JsonMethodsImplicits
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Logger

class GenesisDataLoader(
    blockchainReader: BlockchainReader,
    blockchainWriter: BlockchainWriter,
    stateStorage: StateStorage,
    blockchainConfig: BlockchainConfig
) extends Logger {

  private val bloomLength = 512
  private val hashLength = 64
  private val addressLength = 40

  import Account._

  private val emptyTrieRootHash = ByteString(crypto.kec256(rlp.encode(Array.emptyByteArray)))
  crypto.kec256(ByteString.empty)

  def loadGenesisData(): Unit = {
    log.debug("Loading genesis data")

    val genesisJson = blockchainConfig.customGenesisJsonOpt.getOrElse {
      blockchainConfig.customGenesisFileOpt match {
        case Some(customGenesisFile) =>
          log.debug(s"Trying to load custom genesis data from file: $customGenesisFile")

          Try(Source.fromFile(customGenesisFile)).recoverWith { case _: FileNotFoundException =>
            log.debug(s"Cannot load custom genesis data from file: $customGenesisFile")
            log.debug(s"Trying to load from resources: $customGenesisFile")
            Try(Source.fromResource(customGenesisFile))
          } match {
            case Success(customGenesis) =>
              log.info(s"Using custom genesis data from: $customGenesisFile")
              try customGenesis.getLines().mkString
              finally customGenesis.close()
            case Failure(ex) =>
              log.error(s"Cannot load custom genesis data from: $customGenesisFile", ex)
              throw ex
          }
        case None =>
          log.info("Using default genesis data")
          val src = Source.fromResource("blockchain/default-genesis.json")
          try src.getLines().mkString
          finally src.close()
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
    import org.json4s.native.JsonMethods.parse
    implicit val formats: Formats = DefaultFormats + ByteStringJsonSerializer + UInt256JsonSerializer
    for {
      genesisData <- Try(parse(genesisJson).extract[GenesisData])
      _ <- loadGenesisData(genesisData)
    } yield ()
  }

  def loadGenesisData(genesisData: GenesisData): Try[Unit] = {

    val storage = stateStorage.getReadOnlyStorage
    val initalRootHash = MerklePatriciaTrie.EmptyRootHash

    val stateMptRootHash = getGenesisStateRoot(genesisData, initalRootHash, storage)
    val header: BlockHeader = prepareHeader(genesisData, stateMptRootHash)

    log.debug(s"Prepared genesis header: $header")

    blockchainReader.getBlockHeaderByNumber(0) match {
      case Some(existingGenesisHeader) if existingGenesisHeader.hash == header.hash =>
        log.debug("Genesis data already in the database")
        Success(())
      case Some(_) =>
        Failure(
          new RuntimeException(
            "Genesis data present in the database does not match genesis block from file." +
              " Use different directory for running private blockchains."
          )
        )
      case None =>
        storage.persist()
        stateStorage.forcePersist(GenesisDataLoad)
        blockchainWriter.save(
          Block(header, BlockBody(Nil, Nil)),
          Nil,
          ChainWeight.totalDifficultyOnly(header.difficulty),
          saveAsBestBlock = true
        )
        Success(())
    }
  }

  private def getGenesisStateRoot(genesisData: GenesisData, initalRootHash: Array[Byte], storage: MptStorage) = {
    import MerklePatriciaTrie.defaultByteArraySerializable

    genesisData.alloc.zipWithIndex.foldLeft(initalRootHash) { case (rootHash, ((address, genesisAccount), _)) =>
      val mpt = MerklePatriciaTrie[Array[Byte], Account](rootHash, storage)
      val paddedAddress = address.reverse.padTo(addressLength, "0").reverse.mkString

      val stateRoot = mpt
        .put(
          crypto.kec256(Hex.decode(paddedAddress)),
          Account(
            nonce = genesisAccount.nonce
              .getOrElse(blockchainConfig.accountStartNonce),
            balance = genesisAccount.balance,
            codeHash = genesisAccount.code.fold(Account.EmptyCodeHash)(codeValue => crypto.kec256(codeValue)),
            storageRoot = genesisAccount.storage.fold(Account.EmptyStorageRootHash)(computeStorageRootHash)
          )
        )
        .getRootHash
      stateRoot
    }
  }

  private def computeStorageRootHash(storage: Map[UInt256, UInt256]): ByteString = {
    val emptyTrie = EthereumUInt256Mpt.storageMpt(
      ByteString(MerklePatriciaTrie.EmptyRootHash),
      new SerializingMptStorage(new ArchiveNodeStorage(new NodeStorage(EphemDataSource())))
    )

    val storageTrie = storage.foldLeft(emptyTrie) {
      case (trie, (_, UInt256.Zero)) => trie
      case (trie, (key, value))      => trie.put(key, value)
    }

    ByteString(storageTrie.getRootHash)
  }

  private def prepareHeader(genesisData: GenesisData, stateMptRootHash: Array[Byte]) =
    BlockHeader(
      parentHash = zeros(hashLength),
      ommersHash = ByteString(crypto.kec256(rlp.encode(RLPList()))),
      beneficiary = genesisData.coinbase,
      stateRoot = ByteString(stateMptRootHash),
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
      nonce = genesisData.nonce
    )

  private def zeros(length: Int) =
    ByteString(Hex.decode(List.fill(length)("0").mkString))

}

object GenesisDataLoader {
  object JsonSerializers {

    def deserializeByteString(jv: JValue): ByteString = jv match {
      case JString(s) =>
        val noPrefix = s.replace("0x", "")
        val inp =
          if (noPrefix.length % 2 == 0) noPrefix
          else "0" ++ noPrefix
        Try(ByteString(Hex.decode(inp))) match {
          case Success(bs) => bs
          case Failure(_)  => throw new RuntimeException("Cannot parse hex string: " + s)
        }
      case other => throw new RuntimeException("Expected hex string, but got: " + other)
    }

    object ByteStringJsonSerializer
        extends CustomSerializer[ByteString](_ =>
          (
            { case jv => deserializeByteString(jv) },
            PartialFunction.empty
          )
        )

    def deserializeUint256String(jv: JValue): UInt256 = jv match {
      case JString(s) =>
        Try(UInt256(BigInt(s))) match {
          case Failure(_)     => throw new RuntimeException("Cannot parse hex string: " + s)
          case Success(value) => value
        }
      case other => throw new RuntimeException("Expected hex string, but got: " + other)
    }

    object UInt256JsonSerializer
        extends CustomSerializer[UInt256](_ => ({ case jv => deserializeUint256String(jv) }, PartialFunction.empty))
  }
}

object Implicits extends JsonMethodsImplicits
