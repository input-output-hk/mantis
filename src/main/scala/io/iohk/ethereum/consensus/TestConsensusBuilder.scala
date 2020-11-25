package io.iohk.ethereum.consensus

import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.utils.Logger

/**
  * A [[io.iohk.ethereum.consensus.ConsensusBuilder ConsensusBuilder]] that builds a
  * [[io.iohk.ethereum.consensus.TestConsensus TestConsensus]]
  */
trait TestConsensusBuilder { self: StdConsensusBuilder =>
  protected def buildTestConsensus(): TestConsensus =
    buildConsensus().asInstanceOf[TestConsensus] // we are in tests, so if we get an exception, so be it
}

/** A standard [[TestConsensusBuilder]] cake. */
trait StdTestConsensusBuilder
    extends StdConsensusBuilder
    with TestConsensusBuilder
    with VmBuilder
    with VmConfigBuilder
    with ActorSystemBuilder
    with BlockchainBuilder
    with StorageBuilder
    with BlockchainConfigBuilder
    with NodeKeyBuilder
    with SecureRandomBuilder
    with ConsensusConfigBuilder
    with ShutdownHookBuilder
    with Logger
