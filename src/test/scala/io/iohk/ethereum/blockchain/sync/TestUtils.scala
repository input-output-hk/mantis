package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader

object TestUtils {
  def generateChain(defaultTargetBlockHeader: BlockHeader)(from: BigInt, number: BigInt): Seq[BlockHeader]= {
    val headers = (from until from + number).toSeq.map { nr =>
      defaultTargetBlockHeader.copy(number = nr)
    }

    def genChain(parenthash: ByteString, headers: Seq[BlockHeader], result: Seq[BlockHeader] = Seq.empty): Seq[BlockHeader] = {
      if (headers.isEmpty)
        result
      else {
        val header = headers.head
        val newHeader = header.copy(parentHash = parenthash)
        val newHash = newHeader.hash
        genChain(newHash, headers.tail, result :+ newHeader)
      }
    }

    val first = headers.head

    first +: genChain(first.hash, headers.tail)
  }
}
