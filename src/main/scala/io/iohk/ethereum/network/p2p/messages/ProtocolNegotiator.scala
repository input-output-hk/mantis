package io.iohk.ethereum.network.p2p.messages

import io.iohk.ethereum.network.p2p.Message.Version
import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities
import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities.Eth63Capability

class ProtocolNegotiator(val protocolVersion: Version) {

  val capabilities =
    if (protocolVersion == ProtocolVersions.PV64) Capabilities.All else Seq(Eth63Capability)

  def negotiate(remoteCapabilities: Seq[Capability]): Option[Version] = {
    val commonCapabilities = remoteCapabilities.intersect(capabilities)
    commonCapabilities.sortBy(-_.version).headOption.map(_.version)
  }

}
