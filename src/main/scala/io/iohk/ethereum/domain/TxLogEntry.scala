package io.iohk.ethereum.domain

import akka.util.ByteString
import org.spongycastle.util.encoders.Hex

case class TxLogEntry(loggerAddress: Address, logTopics: Seq[ByteString], data: ByteString) {
  override def toString: String = {
    s"${getClass.getSimpleName}(" +
       s"loggerAddress: $loggerAddress" +
       s"logTopics: ${logTopics.map(e => Hex.toHexString(e.toArray[Byte]))}" +
       s"data: ${Hex.toHexString(data.toArray[Byte])}" + ")"
  }
}
