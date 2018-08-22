package io.iohk.ethereum.bench

import java.io.{File, PrintWriter}

import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.nodebuilder.StorageBuilder
import io.iohk.ethereum.rlp
import org.bouncycastle.util.encoders.Hex
/*
*   Simple program to generate file with rlp encoded blocks in each new line.
*   All paths and settings are taken from main Application.conf file.
*   It requires populated db with best block at least the same as the number of block which supposed to be encoded
*   in file.
*
* */
object FileCreator extends App {
  val blocksToGenerate = 1000000
  val creator = new BlockchainFileBuilder
  creator.generateFile("blocks.txt",  blocksToGenerate)
}

class BlockchainFileBuilder extends StorageBuilder {
  private val blockchainImpl = BlockchainImpl(storagesInstance.storages)
  private val bestBestBlockNr = blockchainImpl.getBestBlockNumber()
  private val newLine = System.getProperty("line.separator")
  
  def generateFile(fileName: String, numBlocks: Int): Unit = {
    require(numBlocks <= bestBestBlockNr)
    val pw = new PrintWriter(new File("blocks.txt" ))
    val blockchainImpl = BlockchainImpl(storagesInstance.storages)

    (0 to numBlocks).foreach {number =>
      val block = blockchainImpl.getBlockByNumber(number).get
      val blockBytes = rlp.encode(block.toRLPEncodable)
      val blockHex = Hex.toHexString(blockBytes)
      pw.append(blockHex)
      pw.append(newLine)
    }

    pw.close()
  }
}