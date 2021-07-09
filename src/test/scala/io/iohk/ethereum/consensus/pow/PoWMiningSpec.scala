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
import io.iohk.ethereum.consensus.FullMiningConfig
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

class PoWMiningSpec
    extends TestKit(ActorSystem("PoWMiningSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers {

  "PoWMining" should "use NoAdditionalPoWData block generator for PoWBlockGeneratorImpl" in new TestSetup {
    val powMining = PoWMining(
      vm,
      storagesInstance.storages.evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      ConsensusConfigs.fullMiningConfig,
      validator,
      NoAdditionalPoWData
    )

    powMining.blockGenerator.isInstanceOf[PoWBlockGeneratorImpl] shouldBe true
  }

  it should "use RestrictedPoWBlockGeneratorImpl block generator for RestrictedPoWMinerData" in new TestSetup {
    val key = mock[AsymmetricCipherKeyPair]

    val powMining = PoWMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      ConsensusConfigs.fullMiningConfig,
      validator,
      RestrictedPoWMinerData(key)
    )

    powMining.blockGenerator.isInstanceOf[RestrictedPoWBlockGeneratorImpl] shouldBe true
  }

  it should "not start a miner when miningEnabled=false" in new TestSetup {
    val configNoMining = miningConfig.copy(miningEnabled = false)
    val fullMiningConfig = FullMiningConfig(configNoMining, ethashConfig)

    val powMining = PoWMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      fullMiningConfig,
      validator,
      NoAdditionalPoWData
    )

    powMining.startProtocol(new TestMiningNode())
    powMining.minerCoordinatorRef shouldBe None
    powMining.mockedMinerRef shouldBe None
  }

  it should "start only one mocked miner when miner protocol is MockedPow" in new TestSetup {
    val configNoMining = miningConfig.copy(miningEnabled = true, protocol = Protocol.MockedPow)
    val fullMiningConfig = FullMiningConfig(configNoMining, ethashConfig)

    val powMining = PoWMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      fullMiningConfig,
      validator,
      NoAdditionalPoWData
    )

    powMining.startProtocol(new TestMiningNode())
    powMining.minerCoordinatorRef shouldBe None
    powMining.mockedMinerRef.isDefined shouldBe true
  }

  it should "start only the normal miner when miner protocol is PoW" in new TestSetup {
    val configNoMining = miningConfig.copy(miningEnabled = true, protocol = Protocol.PoW)
    val fullMiningConfig = FullMiningConfig(configNoMining, ethashConfig)

    val powMining = PoWMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      fullMiningConfig,
      validator,
      NoAdditionalPoWData
    )

    powMining.startProtocol(new TestMiningNode())
    powMining.mockedMinerRef shouldBe None
    powMining.minerCoordinatorRef.isDefined shouldBe true
  }

  it should "start only the normal miner when miner protocol is RestrictedPoW" in new TestSetup {
    val configNoMining = miningConfig.copy(miningEnabled = true, protocol = Protocol.RestrictedPoW)
    val fullMiningConfig = FullMiningConfig(configNoMining, ethashConfig)

    val powMining = PoWMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      fullMiningConfig,
      validator,
      NoAdditionalPoWData
    )

    powMining.startProtocol(new TestMiningNode())
    powMining.mockedMinerRef shouldBe None
    powMining.minerCoordinatorRef.isDefined shouldBe true
  }

  trait TestSetup extends EphemBlockchainTestSetup with MockFactory {
    override lazy val blockchainReader: BlockchainReader = mock[BlockchainReader]
    override lazy val blockchain: BlockchainImpl = mock[BlockchainImpl]
    val evmCodeStorage: EvmCodeStorage = mock[EvmCodeStorage]
    val validator: ValidatorsExecutor = successValidators.asInstanceOf[ValidatorsExecutor]
  }

  class TestMiningNode extends StdNode with EphemBlockchainTestSetup
}
