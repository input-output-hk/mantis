package io.iohk.ethereum.consensus

import io.iohk.ethereum.metrics.MetricsContainer
import io.micrometer.core.instrument.Timer

object ConsensusMetrics extends MetricsContainer {
  final private val blockGenTimer = "consensus.blocks.generate.timer"
  final val EthashBlockGeneratorTiming: Timer = metrics.timer(blockGenTimer, "class", "EthashBlockGenerator")
  final val RestrictedEthashBlockGeneratorTiming: Timer =
    metrics.timer(blockGenTimer, "class", "RestrictedEthashBlockGenerator")
  final val NoOmmersBlockGeneratorTiming: Timer = metrics.timer(blockGenTimer, "class", "NoOmmersBlockGenerator")
}
