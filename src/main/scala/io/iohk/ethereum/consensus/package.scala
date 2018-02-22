package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.domain.{Block, BlockHeader}

/**
 * Provides everything related to consensus.
 * Different consensus protocols are implemented in sub-packages.
 */
package object consensus {
  final type GetBlockHeaderByHash = ByteString ⇒ Option[BlockHeader]
  final type GetNBlocksBack = (ByteString, Int) ⇒ Seq[Block]
}
