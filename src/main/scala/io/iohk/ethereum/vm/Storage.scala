package io.iohk.ethereum.vm

import akka.util.ByteString

/**
  * Account's storage representation. Implementation should be immutable and only keep track of changes to the storage
  */
trait Storage[S <: Storage[S]] {
  def store(offset: ByteString, value: ByteString): S
  def load(offset: ByteString): ByteString
}
