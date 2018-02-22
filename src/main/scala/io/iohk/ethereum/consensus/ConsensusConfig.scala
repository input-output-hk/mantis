package io.iohk.ethereum.consensus

import akka.util.ByteString
import com.typesafe.config.{Config ⇒ TypesafeConfig}
import io.iohk.ethereum.consensus.validators.std.StdBlockHeaderValidator
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.nodebuilder.ShutdownHookBuilder
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.{FiniteDuration, _}

/**
 * Provides generic consensus configuration. Each consensus protocol implementation
 * will use its own specific configuration as well.
 *
 * @param protocol Designates the consensus protocol.
 * @param activeTimeout
 * @param miningEnabled Provides support for generalized "mining". The exact semantics are up to the
 *                      specific consensus protocol implementation.
 */
final case class ConsensusConfig(
  protocol: Protocol,

  coinbase: Address,

  // NOTE Moved from [[io.iohk.ethereum.consensus.ethash.EthashConfig EthashConfig]]
  activeTimeout: FiniteDuration,

  // NOTE Moved from [[io.iohk.ethereum.consensus.ethash.EthashConfig EthashConfig]]
  headerExtraData: ByteString, // only used in BlockGenerator

  // NOTE Moved from [[io.iohk.ethereum.consensus.ethash.EthashConfig EthashConfig]]
  blockCacheSize: Int, // only used in BlockGenerator

  getTransactionFromPoolTimeout: FiniteDuration,

  miningEnabled: Boolean
)

object ConsensusConfig extends Logger {
  object Keys {
    final val Consensus = "consensus"
    final val Protocol = "protocol"
    final val Coinbase = "coinbase"
    final val ActiveTimeout = "active-timeout"
    final val HeaderExtraData = "header-extra-data"
    final val BlockCacheSize = "block-cashe-size"
    final val MiningEnabled = "mining-enabled"
    final val GetTransactionFromPoolTimeout = "get-transaction-from-pool-timeout"
  }


  final val AllowedProtocols = Set(
    Protocol.Names.Ethash,
    Protocol.Names.Demo0,
    Protocol.Names.AtomixRaft
  )

  final val AllowedProtocolsError = (s: String) ⇒ Keys.Consensus +
    " was '" + s + "'" +
    " but it should be one of " +
    AllowedProtocols.map("'" + _ + "'").mkString(",")

  private def readProtocol(consensusConfig: TypesafeConfig): Protocol = {
    val protocol = consensusConfig.getString(Keys.Protocol)

    // If the consensus protocol is not a known one, then it is a fatal error
    // and the application must exit.
    if(!AllowedProtocols(protocol)) {
      val error = AllowedProtocolsError(protocol)
      throw new RuntimeException(error)
    }

    Protocol(protocol)
  }


  def apply(mantisConfig: TypesafeConfig)(shutdownHook: ShutdownHookBuilder): ConsensusConfig = {
    val config = mantisConfig.getConfig(Keys.Consensus)

    def millis(path: String): FiniteDuration = config.getDuration(path).toMillis.millis

    val protocol = shutdownHook.shutdownOnError(readProtocol(config))
    val coinbase = Address(config.getString(Keys.Coinbase))

    val activeTimeout = millis(Keys.ActiveTimeout)
    val headerExtraData = ByteString(config.getString(Keys.HeaderExtraData).getBytes)
      .take(StdBlockHeaderValidator.MaxExtraDataSize)
    val blockCacheSize = config.getInt(Keys.BlockCacheSize)
    val miningEnabled = config.getBoolean(Keys.MiningEnabled)

    val getTransactionFromPoolTimeout = millis(Keys.GetTransactionFromPoolTimeout)

    new ConsensusConfig(
      protocol = protocol,
      coinbase = coinbase,
      activeTimeout = activeTimeout,
      headerExtraData = headerExtraData,
      blockCacheSize = blockCacheSize,
      getTransactionFromPoolTimeout = getTransactionFromPoolTimeout,
      miningEnabled = miningEnabled
    )
  }
}
