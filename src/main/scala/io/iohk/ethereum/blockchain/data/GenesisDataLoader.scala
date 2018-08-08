package io.iohk.ethereum.blockchain.data

import java.io.FileNotFoundException

import akka.util.ByteString
import io.iohk.ethereum.blockchain.data.GenesisDataLoader.JsonSerializers.ByteStringJsonSerializer
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.utils._
import io.iohk.ethereum.{crypto, rlp}
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.utils.EventSupport.EventAttr
import org.json4s.{CustomSerializer, DefaultFormats, Formats, JString, JValue}
import org.spongycastle.util.encoders.Hex

import scala.io.Source
import scala.util.{Failure, Success, Try}

class GenesisDataLoader(
    blockchain: Blockchain,
    blockchainConfig: BlockchainConfig)
  extends Logger with EventSupport {

  private val bloomLength = 512
  private val hashLength = 64
  private val addressLength = 40

  import Account._

  private val emptyTrieRootHash = ByteString(crypto.kec256(rlp.encode(Array.emptyByteArray)))
  private val emptyEvmHash: ByteString = crypto.kec256(ByteString.empty)

  protected def mainService: String = "genesis load"

  def loadGenesisData(): Unit = {
    Event.okStart().send()

    val genesisJson = blockchainConfig.customGenesisFileOpt match {
      case Some(customGenesisFile) =>
        Event.ok().attribute(EventAttr.File, customGenesisFile).send()

        Try(Source.fromFile(customGenesisFile)).recoverWith { case _: FileNotFoundException =>
          Event.warning("file").attribute(EventAttr.File, customGenesisFile).send()
          Try(Source.fromResource(customGenesisFile))
        } match {
          case Success(customGenesis) =>
            Event.ok("file").attribute(EventAttr.File, customGenesisFile).send()
            try {
              customGenesis.getLines().mkString
            } finally {
              customGenesis.close()
            }
          case Failure(ex) =>
            Event.exception(ex).attribute(EventAttr.File, customGenesisFile).send()
            throw ex
        }
      case None =>
        val resource = "blockchain/default-genesis.json"
        val src = Source.fromResource(resource)
        try {
          Event.ok("resource").attribute(EventAttr.Resource, resource).send()
          src.getLines().mkString
        } finally {
          src.close()
        }
    }

    loadGenesisData(genesisJson) match {
      case Success(_) =>
        Event.okFinish().send()
      case Failure(ex) =>
        Event.exceptionFinish(ex).send()
        throw ex
    }
  }

  private def loadGenesisData(genesisJson: String): Try[Unit] = {
    import org.json4s.native.JsonMethods.parse
    implicit val formats: Formats = DefaultFormats + ByteStringJsonSerializer
    for {
      genesisData <- Try(parse(genesisJson).extract[GenesisData])
      _ <- loadGenesisData(genesisData)
    } yield ()
  }

  def loadGenesisData(genesisData: GenesisData): Try[Unit] = {
    import MerklePatriciaTrie.defaultByteArraySerializable

    val ephemDataSource = EphemDataSource()
    val nodeStorage = new NodeStorage(ephemDataSource)
    val initalRootHash = MerklePatriciaTrie.EmptyRootHash

    val stateMptRootHash = genesisData.alloc.zipWithIndex.foldLeft(initalRootHash) { case (rootHash, (((address, AllocAccount(balance)), idx))) =>
      val ephemNodeStorage =  PruningMode.nodesKeyValueStorage(pruning.ArchivePruning, nodeStorage)(Some(idx - genesisData.alloc.size))
      val mpt = MerklePatriciaTrie[Array[Byte], Account](rootHash, ephemNodeStorage)
      val paddedAddress = address.reverse.padTo(addressLength, "0").reverse.mkString
      mpt.put(crypto.kec256(Hex.decode(paddedAddress)),
        Account(blockchainConfig.accountStartNonce, UInt256(BigInt(balance)), emptyTrieRootHash, emptyEvmHash)
      ).getRootHash
    }

    val header: BlockHeader = prepareHeader(genesisData, stateMptRootHash)

    Event(header.toRiemann).send()

    blockchain.getBlockHeaderByNumber(0) match {
      case Some(existingGenesisHeader) if existingGenesisHeader.hash == header.hash =>
        Success(())
      case Some(_) =>
        val exception = new RuntimeException("Genesis data present in the database does not match genesis block from file." +
          " Use different directory for running private blockchains.")
        Event.exception(exception).send()
        Failure(exception)
      case None =>
        ephemDataSource.getAll(nodeStorage.namespace)
          .foreach { case (key, value) => blockchain.saveNode(ByteString(key.toArray[Byte]), value.toArray[Byte], 0) }
        blockchain.save(Block(header, BlockBody(Nil, Nil)), Nil, header.difficulty, saveAsBestBlock = true)
        Success(())
    }
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
      gasLimit = NumericUtils.parseHexOrDecNumber(genesisData.gasLimit),
      gasUsed = 0,
      unixTimestamp = BigInt(genesisData.timestamp.replace("0x", ""), 16).toLong,
      extraData = genesisData.extraData,
      mixHash = genesisData.mixHash.getOrElse(zeros(hashLength)),
      nonce = genesisData.nonce)

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
          case Failure(_) => throw new RuntimeException("Cannot parse hex string: " + s)
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
