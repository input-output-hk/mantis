package io.iohk.ethereum.domain.branch
import akka.util.ByteString

import io.iohk.ethereum.db.storage.BlockNumberMappingStorage
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainReader

/** A Branch instance which only works for the best canonical branch or a subset of this branch.
  * This implementation uses the existing storage indexes to access blocks by number more efficiently.
  */
class BestBranch(
    tipBlockHash: ByteString,
    tipBlockNumber: BigInt,
    bestChainBlockNumberMappingStorage: BlockNumberMappingStorage,
    blockchainReader: BlockchainReader
) extends Branch {}
