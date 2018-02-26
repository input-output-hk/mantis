package io.iohk.ethereum

package object extvm {
  val QueueBufferSize: Int = 16 * 1024
  val LengthPrefixSize: Int = 4

  case class VmDisconnectException(reason: String) extends Exception(reason)
}
