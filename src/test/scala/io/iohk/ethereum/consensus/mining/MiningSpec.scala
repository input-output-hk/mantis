package io.iohk.ethereum.consensus.mining

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MiningSpec extends AnyFlatSpec with Matchers {

  "KnownProtocols" should "have unique names" in {
    val protocols = Protocol.KnownProtocols
    val names = Protocol.KnownProtocolNames

    protocols.size shouldBe names.size
  }

  it should "contain ethash" in {
    Protocol.find(Protocol.PoW.name).isDefined shouldBe true
  }
}
