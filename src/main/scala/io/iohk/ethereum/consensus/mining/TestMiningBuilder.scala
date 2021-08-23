package io.iohk.ethereum.consensus.mining

import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.Logger

/** A [[MiningBuilder]] that builds a [[TestMining]]
  */
trait TestMiningBuilder { self: StdMiningBuilder =>
  protected def buildTestMining(): TestMining =
    buildMining().asInstanceOf[TestMining] // we are in tests, so if we get an exception, so be it
}

/** A standard [[TestMiningBuilder]] cake. */
trait StdTestMiningBuilder
    extends StdMiningBuilder
    with TestMiningBuilder
    with VmBuilder
    with VmConfigBuilder
    with ActorSystemBuilder
    with BlockMetadataProxyBuilder
    with BlockchainBuilder
    with BlockQueueBuilder
    with ConsensusBuilder
    with StorageBuilder
    with BlockchainConfigBuilder
    with NodeKeyBuilder
    with SecureRandomBuilder
    with SyncConfigBuilder
    with MiningConfigBuilder
    with ShutdownHookBuilder
    with Logger
