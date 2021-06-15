package io.iohk.ethereum.forkid

import io.iohk.ethereum.forkid.ForkId._
import io.iohk.ethereum.utils.ForkBlockNumbers
import io.iohk.ethereum.utils.Config._

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should._

class ForkIdSpec extends AnyWordSpec with Matchers {

  val config = blockchains

  "ForkId" must {
    "gatherForks for all chain configurations without errors" in {
      config.blockchains.map { case (name, conf) => (name, gatherForks(conf)) }
    }
    "gatherForks for the etc chain correctly" in {
      val res = config.blockchains.map { case (name, conf) => (name, gatherForks(conf)) }
      res("etc") shouldBe List(1150000, 1920000, 2500000, 3000000, 8772000, 9573000, 10500839, 11700000)
    }

    "gatherForks for the eth chain correctly" in {
      val res = config.blockchains.map { case (name, conf) => (name, gatherForks(conf)) }
      res("eth") shouldBe List(1150000, 1920000, 2463000, 2675000, 4370000, 7280000, 9069000)
    }
  }

}
