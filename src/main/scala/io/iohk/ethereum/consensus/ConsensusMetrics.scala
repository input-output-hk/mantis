package io.iohk.ethereum.consensus

import io.iohk.ethereum.metrics.MetricsContainer

object ConsensusMetrics extends MetricsContainer {
  private final val blockGenTimer = "consensus.blocks.generate.timer"
  final val PoWBlockGeneratorTiming = metrics.timer(blockGenTimer, "class", "PoWBlockGenerator")
  final val RestrictedPoWBlockGeneratorTiming =
    metrics.timer(blockGenTimer, "class", "RestrictedPoWBlockGenerator")
  final val NoOmmersBlockGeneratorTiming = metrics.timer(blockGenTimer, "class", "NoOmmersBlockGenerator")

  final val MinedBlockEvaluationTimer = metrics.timer("consensus.minedblocks.evaluation.timer")
}
