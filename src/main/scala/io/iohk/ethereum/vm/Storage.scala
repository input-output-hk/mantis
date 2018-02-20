package io.iohk.ethereum.vm

/**
  * Account's storage representation. Implementation should be immutable and only keep track of changes to the storage
  */
trait Storage[S <: Storage[S, T], T] {
  def store(offset: T, value: T): S
  def load(offset: T): T
}
