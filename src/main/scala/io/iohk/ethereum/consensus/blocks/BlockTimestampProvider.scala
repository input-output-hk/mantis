package io.iohk.ethereum.consensus.blocks

import java.time.Instant

trait BlockTimestampProvider {
  def getEpochSecond: Long
}

object DefaultBlockTimestampProvider extends BlockTimestampProvider {
  override def getEpochSecond: Long = Instant.now.getEpochSecond
}
