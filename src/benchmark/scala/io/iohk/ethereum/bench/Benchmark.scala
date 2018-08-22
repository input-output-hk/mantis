package io.iohk.ethereum.bench

//import io.iohk.ethereum.domain.BlockchainImpl
import java.io.File
import java.util.concurrent.Executors

import akka.actor.ActorSystem
import io.iohk.ethereum.consensus.{ConsensusConfigBuilder, StdConsensusBuilder}
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.Block._
import io.iohk.ethereum.ledger.{BlockImportedToTop, DuplicateBlock}
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.utils.{Config, Logger}
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
/*
* Simple program to benchmark block import performance.
* It requires file with rlp encoded blocks to be located in main mantis dir
* All processed data will be saved to dir specified in rocksdb config specified in main Application.conf
* Of cource block import is not whole story of mantis performance but it is great part of it.
*
*
* */
//scalastyle:off
object Benchmark extends App with GenesisDataLoaderBuilder
  with Logger
  with ActorSystemBuilder
  with StorageBuilder
  with BlockchainConfigBuilder
  with BlockchainBuilder
  with VmConfigBuilder
  with VmBuilder
  with SyncConfigBuilder
  with ShutdownHookBuilder
  with ConsensusConfigBuilder
  with StdConsensusBuilder
  with StdLedgerBuilder {

  override implicit lazy val system = ActorSystem("Test_System")

  prepareDbDir(Config.Db.RocksDb.path)

  val exe = Executors.newFixedThreadPool(4)

  val context = ExecutionContext.fromExecutor(exe)

  val source = Source.fromFile("blocks.txt")

  genesisDataLoader.loadGenesisData()

  val lines = source.getLines()

  lines.foreach { line =>
    val block = Hex.decode(line).toBlock
    val fut = Future {
      importBlock(block)
    }(context)
    Await.ready(fut, Duration.Inf)
  }

  val best = blockchain.getBestBlockNumber()

  print(best)

  source.close()

  shutdown()

  def importBlock(block: Block): Unit = {
    val result = ledger.importBlock(block)
    result match {
      case BlockImportedToTop(blocks, diffs) =>
        assert(blocks.size == 1)
        assert(blocks.head == block)
        println(s"Block ${blocks.head.header.number} imported")
      case DuplicateBlock => ()
      case _ => throw new RuntimeException("test failed")
    }
  }

  def prepareDbDir(dbPath: String): Unit = {
    val file =  new File(dbPath)

    if (!file.exists()) {
      file.mkdirs()
    } else {
      if (file.listFiles().length > 0) {
        throw new RuntimeException("Database directory should be empty")
      }
    }
  }

}
