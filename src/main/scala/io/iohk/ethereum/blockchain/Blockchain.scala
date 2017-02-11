package io.iohk.ethereum.blockchain

/**
  * Entity to be used to persist and query  Blockchain related objects (blocks, transactions, ommers)
  */
trait Blockchain extends BlocksRepository with ReceiptsRepository with EvmRepository
