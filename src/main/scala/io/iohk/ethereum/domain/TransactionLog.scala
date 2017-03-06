package io.iohk.ethereum.domain

import akka.util.ByteString
import org.spongycastle.util.encoders.Hex

case class TransactionLog(loggerAddress: ByteString, logTopics: Seq[ByteString], data: ByteString) {
  override def toString: String = {
    s"""TransactionLog{
       |loggerAddress: ${Hex.toHexString(loggerAddress.toArray[Byte])}
       |logTopics: ${logTopics.map(e => Hex.toHexString(e.toArray[Byte]))}
       |data: ${Hex.toHexString(data.toArray[Byte])}
       |}
       """.stripMargin
  }
}
