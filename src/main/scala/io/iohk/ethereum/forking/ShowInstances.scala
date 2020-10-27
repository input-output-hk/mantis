package io.iohk.ethereum.forking
import akka.util.ByteString
import cats.Show
import io.iohk.ethereum.utils.ByteStringUtils

object ShowInstances {
  implicit val byteStringShow: Show[ByteString] = ByteStringUtils.hash2string(_)
}
