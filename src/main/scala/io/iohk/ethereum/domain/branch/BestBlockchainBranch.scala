package io.iohk.ethereum.domain.branch
import akka.util.ByteString

import io.iohk.ethereum.db.storage.BlockNumberMappingStorage
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainReader

/** A Branch instance which only works for the best canonical branch. As this branch
  * currently has specific indexes (particularly regarding accessing blocks by number),
  * it will uses thoses to provide better performance.
  */
class BestBlockchainBranch(
    tipBlockHeader: BlockHeader,
    bestChainBlockNumberMappingStorage: BlockNumberMappingStorage,
    blockchainReader: BlockchainReader
) extends BlockchainBranch {

  /* The following assumptions are made in this class :
   *  - the whole branch exist in storage
   *  - The various metadata and index are consistent
   *
   * If those assumptions are found to be false, then the application is in an inconsistent
   * state and the class will throw.
   * */

  override def getBlockByNumber(number: BigInt): Option[Block] =
    if (tipBlockHeader.number >= number && number >= 0) {
      for {
        hash <- getHashByBlockNumber(number)
        block <- blockchainReader.getBlockByHash(hash)
      } yield block
    } else None

  override def getHashByBlockNumber(number: BigInt): Option[ByteString] =
    bestChainBlockNumberMappingStorage.get(number)
}
