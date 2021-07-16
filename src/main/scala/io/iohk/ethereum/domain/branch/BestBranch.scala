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
    tipBlockHeader: BlockHeader,
    bestChainBlockNumberMappingStorage: BlockNumberMappingStorage,
    blockchainReader: BlockchainReader
) extends Branch {

  /* The following assumptions are made in this class :
   *  - The whole branch exists in storage
   *  - The various metadata and index are consistent
   */

  override def getBlockByNumber(number: BigInt): Option[Block] =
    if (tipBlockHeader.number >= number && number >= 0) {
      for {
        hash <- getHashByBlockNumber(number)
        block <- blockchainReader.getBlockByHash(hash)
      } yield block
    } else None

  override def getHashByBlockNumber(number: BigInt): Option[ByteString] =
    if (tipBlockHeader.number >= number && number >= 0) {
      bestChainBlockNumberMappingStorage.get(number)
    } else None
}
