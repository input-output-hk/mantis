package io.iohk.ethereum.consensus.mining

import akka.util.ByteString

import com.typesafe.config.{Config => TypesafeConfig}

import io.iohk.ethereum.consensus.validators.BlockHeaderValidator
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.Logger

/** Provides generic mining configuration. Each consensus protocol implementation
  * will use its own specific configuration as well.
  *
  * @param protocol Designates the mining protocol.
  * @param miningEnabled Provides support for generalized "mining". The exact semantics are up to the
  *                      specific mining protocol implementation.
  */
final case class MiningConfig(
    protocol: Protocol,
    coinbase: Address,
    headerExtraData: ByteString, // only used in BlockGenerator
    blockCacheSize: Int, // only used in BlockGenerator
    miningEnabled: Boolean
)

object MiningConfig extends Logger {
  object Keys {
    final val Mining = "mining"
    final val Protocol = "protocol"
    final val Coinbase = "coinbase"
    final val HeaderExtraData = "header-extra-data"
    final val BlockCacheSize = "block-cashe-size"
    final val MiningEnabled = "mining-enabled"
  }

  final val AllowedProtocols: Set[String] = Set(
    Protocol.Names.PoW,
    Protocol.Names.MockedPow,
    Protocol.Names.RestrictedPoW
  )

  final val AllowedProtocolsError: String => String = (s: String) =>
    Keys.Mining +
      " is configured as '" + s + "'" +
      " but it should be one of " +
      AllowedProtocols.map("'" + _ + "'").mkString(",")

  private def readProtocol(miningConfig: TypesafeConfig): Protocol = {
    val protocol = miningConfig.getString(Keys.Protocol)

    // If the mining protocol is not a known one, then it is a fatal error
    // and the application must exit.
    if (!AllowedProtocols(protocol)) {
      val error = AllowedProtocolsError(protocol)
      throw new RuntimeException(error)
    }

    Protocol(protocol)
  }

  def apply(mantisConfig: TypesafeConfig): MiningConfig = {
    val config = mantisConfig.getConfig(Keys.Mining)

    val protocol = readProtocol(config)
    val coinbase = Address(config.getString(Keys.Coinbase))

    val headerExtraData = ByteString(config.getString(Keys.HeaderExtraData).getBytes)
      .take(BlockHeaderValidator.MaxExtraDataSize)
    val blockCacheSize = config.getInt(Keys.BlockCacheSize)
    val miningEnabled = config.getBoolean(Keys.MiningEnabled)

    new MiningConfig(
      protocol = protocol,
      coinbase = coinbase,
      headerExtraData = headerExtraData,
      blockCacheSize = blockCacheSize,
      miningEnabled = miningEnabled
    )
  }
}
