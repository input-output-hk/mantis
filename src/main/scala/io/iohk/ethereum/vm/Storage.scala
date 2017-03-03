package io.iohk.ethereum.vm


/**
  * Account's storage representation. Implementation should be immutable and only keep track of changes to the storage
  */
trait Storage[S <: Storage[S]] {
  def store(addr: DataWord, value: DataWord): S
  def load(addr: DataWord): DataWord
}
