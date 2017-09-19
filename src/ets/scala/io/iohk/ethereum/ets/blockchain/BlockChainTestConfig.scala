package io.iohk.ethereum.ets.blockchain

import akka.util.ByteString
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.utils.{BlockchainConfig, MonetaryPolicyConfig}

trait BlockChainTestConfig extends BlockchainConfig {

  val frontierBlockNumber: BigInt = Long.MaxValue
  val eip160BlockNumber: BigInt = Long.MaxValue
  val eip150BlockNumber: BigInt = Long.MaxValue
  val eip155BlockNumber: BigInt = Long.MaxValue
  val homesteadBlockNumber: BigInt = Long.MaxValue
  //Enabling maxGasLimit in all Configs and all blocks
  override val eip106BlockNumber: BigInt = 0
  // unused
  override val maxCodeSize: Option[BigInt] = None
  override val difficultyBombPauseBlockNumber: BigInt = 3000000
  override val difficultyBombContinueBlockNumber: BigInt = 5000000
  override val chainId: Byte = 0x3d.toByte
  override val customGenesisFileOpt: Option[String] = Some("test-genesis.json")
  override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"))
  override val daoForkBlockNumber: BigInt = Long.MaxValue
  override val daoForkBlockHash: ByteString = ByteString("unused")
  override val accountStartNonce: UInt256 = UInt256.Zero
}

class FrontierConfig extends BlockChainTestConfig {
  override val frontierBlockNumber = 0
}
class HomesteadConfig extends BlockChainTestConfig {
  override val homesteadBlockNumber = 0
}
class Eip150Config extends BlockChainTestConfig {

  // To keep difficulty calculation relevant, we need to have network order
  // frontier >> homestead >> eip150
  override val homesteadBlockNumber: BigInt = -1
  override val eip150BlockNumber = 0
}
class FrontierToHomesteadAt5 extends BlockChainTestConfig {
  override val frontierBlockNumber = 0
  override val homesteadBlockNumber = 5
}
class HomesteadToEIP150At5 extends BlockChainTestConfig {
  override val eip150BlockNumber = 5
  override val homesteadBlockNumber = 0
}

