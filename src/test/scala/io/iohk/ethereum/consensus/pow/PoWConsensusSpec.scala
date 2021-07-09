package io.iohk.ethereum.consensus.pow

import akka.actor.ActorSystem
import akka.testkit.TestKit

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.ConsensusConfigs
import io.iohk.ethereum.consensus.ConsensusConfigs.ethashConfig
import io.iohk.ethereum.consensus.FullConsensusConfig
import io.iohk.ethereum.consensus.Protocol
import io.iohk.ethereum.consensus.Protocol.NoAdditionalPoWData
import io.iohk.ethereum.consensus.Protocol.RestrictedPoWMinerData
import io.iohk.ethereum.consensus.pow.blocks.PoWBlockGeneratorImpl
import io.iohk.ethereum.consensus.pow.blocks.RestrictedPoWBlockGeneratorImpl
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.nodebuilder.StdNode

class PoWConsensusSpec
    extends TestKit(ActorSystem("PoWConsensusSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers {

  "PoWConsensus" should "use NoAdditionalPoWData block generator for PoWBlockGeneratorImpl" in new TestSetup {
    val powConsensus = PoWMining(
      vm,
      storagesInstance.storages.evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      ConsensusConfigs.fullConsensusConfig,
      validator,
      NoAdditionalPoWData
    )

    powConsensus.blockGenerator.isInstanceOf[PoWBlockGeneratorImpl] shouldBe true
  }

  it should "use RestrictedPoWBlockGeneratorImpl block generator for RestrictedPoWMinerData" in new TestSetup {
    val key = mock[AsymmetricCipherKeyPair]

    val powConsensus = PoWMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      ConsensusConfigs.fullConsensusConfig,
      validator,
      RestrictedPoWMinerData(key)
    )

    powConsensus.blockGenerator.isInstanceOf[RestrictedPoWBlockGeneratorImpl] shouldBe true
  }

  it should "not start a miner when miningEnabled=false" in new TestSetup {
    val configNoMining = consensusConfig.copy(miningEnabled = false)
    val fullConsensusConfig = FullConsensusConfig(configNoMining, ethashConfig)

    val powConsensus = PoWMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      fullConsensusConfig,
      validator,
      NoAdditionalPoWData
    )

    powConsensus.startProtocol(new TestConsensusNode())
    powConsensus.minerCoordinatorRef shouldBe None
    powConsensus.mockedMinerRef shouldBe None
  }

  it should "start only one mocked miner when miner protocol is MockedPow" in new TestSetup {
    val configNoMining = consensusConfig.copy(miningEnabled = true, protocol = Protocol.MockedPow)
    val fullConsensusConfig = FullConsensusConfig(configNoMining, ethashConfig)

    val powConsensus = PoWMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      fullConsensusConfig,
      validator,
      NoAdditionalPoWData
    )

    powConsensus.startProtocol(new TestConsensusNode())
    powConsensus.minerCoordinatorRef shouldBe None
    powConsensus.mockedMinerRef.isDefined shouldBe true
  }

  it should "start only the normal miner when miner protocol is PoW" in new TestSetup {
    val configNoMining = consensusConfig.copy(miningEnabled = true, protocol = Protocol.PoW)
    val fullConsensusConfig = FullConsensusConfig(configNoMining, ethashConfig)

    val powConsensus = PoWMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      fullConsensusConfig,
      validator,
      NoAdditionalPoWData
    )

    powConsensus.startProtocol(new TestConsensusNode())
    powConsensus.mockedMinerRef shouldBe None
    powConsensus.minerCoordinatorRef.isDefined shouldBe true
  }

  it should "start only the normal miner when miner protocol is RestrictedPoW" in new TestSetup {
    val configNoMining = consensusConfig.copy(miningEnabled = true, protocol = Protocol.RestrictedPoW)
    val fullConsensusConfig = FullConsensusConfig(configNoMining, ethashConfig)

    val powConsensus = PoWMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      fullConsensusConfig,
      validator,
      NoAdditionalPoWData
    )

    powConsensus.startProtocol(new TestConsensusNode())
    powConsensus.mockedMinerRef shouldBe None
    powConsensus.minerCoordinatorRef.isDefined shouldBe true
  }

  trait TestSetup extends EphemBlockchainTestSetup with MockFactory {
    override lazy val blockchainReader: BlockchainReader = mock[BlockchainReader]
    override lazy val blockchain: BlockchainImpl = mock[BlockchainImpl]
    val evmCodeStorage: EvmCodeStorage = mock[EvmCodeStorage]
    val validator: ValidatorsExecutor = successValidators.asInstanceOf[ValidatorsExecutor]
  }

  class TestConsensusNode extends StdNode with EphemBlockchainTestSetup
}
