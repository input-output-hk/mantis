package io.iohk.ethereum.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigSpec extends AnyFlatSpec with Matchers {
  "clientId" should "by default come from VersionInfo" in {
    Config.clientId shouldBe VersionInfo.clientId
  }
}
