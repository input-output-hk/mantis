package io.iohk.ethereum.network.p2p.messages

import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities
import io.iohk.ethereum.network.p2p.messages.Capability.Capabilities.Eth63Capability
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ProtocolNegotiatorSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {

  private val protocolNegotiatorPV63 = new ProtocolNegotiator(ProtocolVersions.PV63)
  private val protocolNegotiatorPV64 = new ProtocolNegotiator(ProtocolVersions.PV64)
  private val remoteCapabilities = Capability("eth", 65) +: Capabilities.All
  private val eth64Capability = Capability("eth", 64)

  "Protocol negotiator" when {
    s"protocol version is ${ProtocolVersions.PV63}" should {
      s"return ${ProtocolVersions.PV63} as possible capability" in {
        protocolNegotiatorPV63.capabilities shouldBe Seq(Eth63Capability)
      }

      s"return ${ProtocolVersions.PV63} as negotiated protocol" in {
        protocolNegotiatorPV63.negotiate(remoteCapabilities) shouldBe Some(ProtocolVersions.PV63)
      }

      s"return no negotiated protocol" in {
        val peerCapabilities = Seq(eth64Capability)
        protocolNegotiatorPV63.negotiate(peerCapabilities) shouldBe None
      }
    }

    s"protocol version is ${ProtocolVersions.PV64}" should {
      s"return ${Capabilities.All} as possible capabilities" in {
        protocolNegotiatorPV64.capabilities shouldBe Capabilities.All
      }

      s"return ${ProtocolVersions.PV64} as negotiated protocol" in {
        protocolNegotiatorPV64.negotiate(remoteCapabilities) shouldBe Some(ProtocolVersions.PV64)
      }

      s"return ${ProtocolVersions.PV63} as negotiated protocol" in {
        val peerCapabilities = eth64Capability +: remoteCapabilities.filterNot(_.name == "etc")
        protocolNegotiatorPV64.negotiate(peerCapabilities) shouldBe Some(ProtocolVersions.PV63)
      }

      s"return no negotiated protocol" in {
        val peerCapabilities = Seq(eth64Capability)
        protocolNegotiatorPV64.negotiate(peerCapabilities) shouldBe None
      }
    }
  }
}
