package io.iohk.ethereum.vm

/** Account's storage representation. Implementation should be immutable and only keep track of changes to the storage
  */
trait Storage[S <: Storage[S]] {
  def store(offset: BigInt, value: BigInt): S
  def load(offset: BigInt): BigInt
}
