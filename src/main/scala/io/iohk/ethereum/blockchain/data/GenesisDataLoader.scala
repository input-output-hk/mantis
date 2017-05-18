package io.iohk.ethereum.blockchain.data

import akka.util.ByteString
import io.iohk.ethereum.blockchain.data.GenesisDataLoader.JsonSerializers.ByteStringJsonSerializer
import io.iohk.ethereum.network.p2p.messages.PV63.AccountImplicits
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.{crypto, rlp}
import io.iohk.ethereum.db.dataSource.{DataSource, EphemDataSource}
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.domain.{Account, Block, BlockHeader, Blockchain}
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.vm.UInt256
import org.json4s.{CustomSerializer, DefaultFormats, JString, JValue}
import org.spongycastle.util.encoders.Hex

import scala.io.Source
import scala.util.{Failure, Success, Try}

class GenesisDataLoader(
    dataSource: DataSource,
    blockchain: Blockchain,
    blockchainConfig: BlockchainConfig)
  extends Logger{

  private val bloomLength = 512
  private val hashLength = 64
  private val addressLength = 40

  import Account._

  private val emptyTrieRootHash = ByteString(crypto.kec256(rlp.encode(Array.emptyByteArray)))
  private val emptyEvmHash: ByteString = crypto.kec256(ByteString.empty)

  def loadGenesisData(): Unit = {
    log.info("Loading genesis data")

    val genesisJson = blockchainConfig.customGenesisFileOpt match {
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
    import org.json4s.native.JsonMethods.parse
    implicit val formats = DefaultFormats + ByteStringJsonSerializer
    for {
      genesisData <- Try(parse(genesisJson).extract[GenesisData])
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
      mpt.put(crypto.kec256(Hex.decode(paddedAddress)), Account(0, UInt256(BigInt(balance)), emptyTrieRootHash, emptyEvmHash))
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
        // using empty namespace because ephemDataSource.storage already has the namespace-prefixed keys
        dataSource.update(IndexedSeq(), Nil, ephemDataSource.storage.toSeq)
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
  object JsonSerializers {

    def deserializeByteString(jv: JValue): ByteString = jv match {
      case JString(s) =>
        val noPrefix = s.replace("0x", "")
        val inp =
          if (noPrefix.length % 2 == 0) noPrefix
          else "0" ++ noPrefix
        Try(ByteString(Hex.decode(inp))) match {
          case Success(bs) => bs
          case Failure(ex) => throw new RuntimeException("Cannot parse hex string: " + s)
        }
      case other => throw new RuntimeException("Expected hex string, but got: " + other)
    }

    object ByteStringJsonSerializer extends CustomSerializer[ByteString](formats =>
      (
        { case jv => deserializeByteString(jv) },
        PartialFunction.empty
      )
    )

  }
}
