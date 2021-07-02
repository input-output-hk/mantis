package io.iohk.ethereum.consensus

import io.micrometer.core.instrument.Timer

import io.iohk.ethereum.metrics.MetricsContainer

object ConsensusMetrics extends MetricsContainer {
  final private val blockGenTimer = "consensus.blocks.generate.timer"
  final val PoWBlockGeneratorTiming: Timer = metrics.timer(blockGenTimer, "class", "PoWBlockGenerator")
  final val RestrictedPoWBlockGeneratorTiming: Timer =
    metrics.timer(blockGenTimer, "class", "RestrictedPoWBlockGenerator")
  final val NoOmmersBlockGeneratorTiming: Timer = metrics.timer(blockGenTimer, "class", "NoOmmersBlockGenerator")

  final val MinedBlockEvaluationTimer: Timer = metrics.timer("consensus.minedblocks.evaluation.timer")
}
