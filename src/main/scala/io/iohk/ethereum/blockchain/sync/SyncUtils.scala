package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.domain.BlockHeader

trait SyncUtils {

  def checkHeaders(headers: Seq[BlockHeader]): Boolean =
    if (headers.length > 1) headers.zip(headers.tail).forall { case (parent, child) => parent.hash == child.parentHash && parent.number + 1 == child.number }
    else true

}
