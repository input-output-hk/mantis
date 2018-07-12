package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.rlp.{RLPDecoder, RLPList}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._

import scala.util.Try

package object vm {
  /**
    * Number of 32-byte UInt256s required to hold n bytes (~= math.ceil(n / 32))
    */
  def wordsForBytes(n: BigInt): BigInt =
    if (n == 0) 0 else (n - 1) / UInt256.Size + 1

  def isValidIeleCall(payload: ByteString): Boolean =
    Try(rlp.decode[IeleTxData](payload.toArray[Byte])).isSuccess

  private case class IeleTxData(functionName: String, args: Seq[ByteString])
  private object IeleTxData {
    implicit val rlpDec: RLPDecoder[IeleTxData] = {
      case RLPList(functionName, args: RLPList) => IeleTxData(functionName, args.items.map(byteStringEncDec.decode))
      case _ => throw new RuntimeException("Invalid IeleTxData RLP")
    }
  }


}
