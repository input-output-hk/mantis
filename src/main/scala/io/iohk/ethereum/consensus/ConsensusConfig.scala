package io.iohk.ethereum.consensus

import com.typesafe.config.{Config ⇒ TypesafeConfig}
import io.iohk.ethereum.nodebuilder.ShutdownHookBuilder
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration.{FiniteDuration, _}

/**
 *
 * @param protocol Designates the consensus protocol.
 * @param activeTimeout
 * @param miningEnabled Provides support for generalized "mining".
 *                      If this is `true` then the consensus protocol implementation must be notified via
 *                      [[io.iohk.ethereum.consensus.Consensus#startMiningProcess Consensus.startMiningProcess]]
 *                      in order to start its mining process.
 */
final case class ConsensusConfig(
  protocol: Protocol,

  // NOTE Moved from [[io.iohk.ethereum.consensus.ethash.MiningConfig MiningConfig]]
  activeTimeout: FiniteDuration,

  miningEnabled: Boolean
)

object ConsensusConfig extends Logger {
  object Keys {
    final val Consensus = "consensus"
    final val Protocol = "protocol"
    final val ActiveTimeout = "active-timeout"
    final val MiningEnabled = "mining-enabled"
  }


  final val AllowedProtocols = Set(Protocol.Names.Ethash, Protocol.Names.DemoConsensus)
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


  def apply(etcClientConfig: TypesafeConfig)(shutdownHook: ShutdownHookBuilder): ConsensusConfig = {
    val config = etcClientConfig.getConfig(Keys.Consensus)

    val protocol = shutdownHook.shutdownOnError(readProtocol(config))
    val activeTimeout = config.getDuration(Keys.ActiveTimeout).toMillis.millis
    val miningEnabled = config.getBoolean(Keys.MiningEnabled)

    new ConsensusConfig(
      protocol = protocol,
      activeTimeout = activeTimeout,
      miningEnabled = miningEnabled
    )
  }
}
