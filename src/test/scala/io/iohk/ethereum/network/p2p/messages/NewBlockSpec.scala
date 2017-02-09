package io.iohk.ethereum.network.p2p.messages

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import org.scalatest.prop.PropertyChecks
import org.scalatest.FunSuite
import io.iohk.ethereum.rlp.{decode, encode}

class NewBlockSpec extends FunSuite with PropertyChecks  with ObjectGenerators {
  test("NewBlock messages are encoded and decoded properly") {
    forAll(newBlockGen) { newBlock =>
      val encoded = encode(newBlock)
      val decoded: NewBlock = decode[NewBlock](encoded)
      assert(decoded == newBlock)
    }
  }
}
