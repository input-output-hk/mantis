package io.iohk.ethereum.forkid

import java.util.zip.CRC32
import java.nio.ByteBuffer

import akka.util.ByteString
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.ByteUtils._
import io.iohk.ethereum.rlp._

import RLPImplicitConversions._

case class ForkId(hash: BigInt, next: Option[BigInt])

object ForkId {

  def create(genesisHash: ByteString, config: BlockchainConfig)(head: BigInt): ForkId = {
    val crc = new CRC32()
    crc.update(genesisHash.asByteBuffer)
    val next = gatherForks(config).find { fork =>
      if (fork <= head) {
        crc.update(bigIntToBytes(fork, 8))
      }
      fork > head
    }
    new ForkId(crc.getValue(), next)
  }

  val noFork = BigInt("1000000000000000000")

  def gatherForks(config: BlockchainConfig): List[BigInt] = {
    val maybeDaoBlock: Option[BigInt] = config.daoForkConfig.flatMap { daoConf =>
      if (daoConf.includeOnForkIdList) Some(daoConf.forkBlockNumber)
      else None
    }

    (maybeDaoBlock.toList ++
      config.forkBlockNumbers.productIterator.toList.flatMap {
        case i: BigInt => Some(i)
        case i: Option[_] =>
          i.flatMap {
            case n if n.isInstanceOf[BigInt] => Some(n.asInstanceOf[BigInt])
            case n => None
          }
        case default => None
      })
      .filterNot(v => v == 0 || v == noFork)
      .distinct
      .sorted
  }

  implicit class ForkIdEnc(forkId: ForkId) extends RLPSerializable {
    import RLPImplicits._

    import io.iohk.ethereum.utils.ByteUtils._
    override def toRLPEncodable: RLPEncodeable = {
      val hash: Array[Byte] = bigIntToBytes(forkId.hash, 4).takeRight(4)
      val next: Array[Byte] = bigIntToUnsignedByteArray(forkId.next.getOrElse(BigInt(0))).takeRight(8)
      RLPList(hash, next)
    }

  }

  implicit val forkIdEnc = new RLPDecoder[ForkId] {

    def decode(rlp: RLPEncodeable): ForkId = rlp match {
      case RLPList(hash, next) => {
        val i = bigIntFromEncodeable(next)
        ForkId(bigIntFromEncodeable(hash), if (i == 0) None else Some(i))
      }
      case _ => throw new RuntimeException("Error when decoding ForkId")
    }
  }
}
