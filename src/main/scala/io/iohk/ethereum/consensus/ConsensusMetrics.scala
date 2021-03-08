package io.iohk.ethereum.consensus

import io.iohk.ethereum.metrics.MetricsContainer

object ConsensusMetrics extends MetricsContainer {
  final private val blockGenTimer = "consensus.blocks.generate.timer"
  final val EthashBlockGeneratorTiming = metrics.timer(blockGenTimer, "class", "EthashBlockGenerator")
  final val RestrictedEthashBlockGeneratorTiming =
    metrics.timer(blockGenTimer, "class", "RestrictedEthashBlockGenerator")
  final val NoOmmersBlockGeneratorTiming = metrics.timer(blockGenTimer, "class", "NoOmmersBlockGenerator")
}
