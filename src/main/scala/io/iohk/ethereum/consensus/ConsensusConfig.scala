package io.iohk.ethereum.consensus

import akka.util.ByteString
import com.typesafe.config.{Config => TypesafeConfig}
import io.iohk.ethereum.consensus.validators.BlockHeaderValidator
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.Logger

/**
  * Provides generic consensus configuration. Each consensus protocol implementation
  * will use its own specific configuration as well.
  *
  * @param protocol Designates the consensus protocol.
  * @param miningEnabled Provides support for generalized "mining". The exact semantics are up to the
  *                      specific consensus protocol implementation.
  */
final case class ConsensusConfig(
    protocol: Protocol,
    coinbase: Address,
    headerExtraData: ByteString, // only used in BlockGenerator
    blockCacheSize: Int, // only used in BlockGenerator
    miningEnabled: Boolean,
    treasuryOptOut: Boolean
)

object ConsensusConfig extends Logger {
  object Keys {
    final val Consensus = "consensus"
    final val Protocol = "protocol"
    final val Coinbase = "coinbase"
    final val HeaderExtraData = "header-extra-data"
    final val BlockCacheSize = "block-cashe-size"
    final val MiningEnabled = "mining-enabled"
    final val TreasuryOptOut = "treasury-opt-out"
  }

  final val AllowedProtocols = Set(
    Protocol.Names.Ethash,
    Protocol.Names.MockedPow,
    Protocol.Names.RestrictedEthash
  )

  final val AllowedProtocolsError = (s: String) =>
    Keys.Consensus +
      " is configured as '" + s + "'" +
      " but it should be one of " +
      AllowedProtocols.map("'" + _ + "'").mkString(",")

  private def readProtocol(consensusConfig: TypesafeConfig): Protocol = {
    val protocol = consensusConfig.getString(Keys.Protocol)

    // If the consensus protocol is not a known one, then it is a fatal error
    // and the application must exit.
    if (!AllowedProtocols(protocol)) {
      val error = AllowedProtocolsError(protocol)
      throw new RuntimeException(error)
    }

    Protocol(protocol)
  }

  def apply(mantisConfig: TypesafeConfig): ConsensusConfig = {
    val config = mantisConfig.getConfig(Keys.Consensus)

    val protocol = readProtocol(config)
    val coinbase = Address(config.getString(Keys.Coinbase))

    val headerExtraData = ByteString(config.getString(Keys.HeaderExtraData).getBytes)
      .take(BlockHeaderValidator.MaxExtraDataSize)
    val blockCacheSize = config.getInt(Keys.BlockCacheSize)
    val miningEnabled = config.getBoolean(Keys.MiningEnabled)
    val optOut = config.getBoolean(Keys.TreasuryOptOut)

    new ConsensusConfig(
      protocol = protocol,
      coinbase = coinbase,
      headerExtraData = headerExtraData,
      blockCacheSize = blockCacheSize,
      miningEnabled = miningEnabled,
      treasuryOptOut = optOut
    )
  }
}
