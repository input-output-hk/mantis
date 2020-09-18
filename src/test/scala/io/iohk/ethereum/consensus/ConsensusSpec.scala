package io.iohk.ethereum.consensus

import org.scalatest.{FlatSpec, Matchers}

class ConsensusSpec extends FlatSpec with Matchers {

  "KnownProtocols" should "have unique names" in {
    val protocols = Protocol.KnownProtocols
    val names = Protocol.KnownProtocolNames

    protocols.size shouldBe names.size
  }

  it should "contain ethash" in {
    Protocol.find(Protocol.Ethash.name).isDefined shouldBe true
  }
}
