package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.messages.PV63.TxLogEntryImplicits
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPEncoder, RLPList}
import org.spongycastle.util.encoders.Hex

case class Receipt(
                    postTransactionStateHash: ByteString,
                    cumulativeGasUsed: BigInt,
                    logsBloomFilter: ByteString,
                    logs: Seq[TxLogEntry]
                  ) {
  override def toString: String = {
    s"""
       |Receipt{
       |postTransactionStateHash: ${Hex.toHexString(postTransactionStateHash.toArray[Byte])}
       |cumulativeGasUsed: $cumulativeGasUsed
       |logsBloomFilter: ${Hex.toHexString(logsBloomFilter.toArray[Byte])}
       |logs: $logs
       |}
       """.stripMargin
  }
}
