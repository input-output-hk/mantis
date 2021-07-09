package io.iohk.ethereum.consensus

import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.Logger

/** A [[io.iohk.ethereum.consensus.MiningBuilder ConsensusBuilder]] that builds a
  * [[io.iohk.ethereum.consensus.TestMining TestConsensus]]
  */
trait TestMiningBuilder { self: StdMiningBuilder =>
  protected def buildTestConsensus(): TestMining =
    buildMining().asInstanceOf[TestMining] // we are in tests, so if we get an exception, so be it
}

/** A standard [[TestMiningBuilder]] cake. */
trait StdTestConsensusBuilder
    extends StdMiningBuilder
    with TestMiningBuilder
    with VmBuilder
    with VmConfigBuilder
    with ActorSystemBuilder
    with BlockchainBuilder
    with BlockQueueBuilder
    with BlockImportBuilder
    with StorageBuilder
    with BlockchainConfigBuilder
    with NodeKeyBuilder
    with SecureRandomBuilder
    with SyncConfigBuilder
    with ConsensusConfigBuilder
    with ShutdownHookBuilder
    with Logger
