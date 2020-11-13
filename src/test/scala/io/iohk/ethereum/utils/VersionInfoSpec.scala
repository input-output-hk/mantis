package io.iohk.ethereum.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VersionInfoSpec extends AnyFlatSpec with Matchers {
  behavior of "clientId"

  it should "preserve major and minor Java version" in {
    VersionInfo.clientId should fullyMatch regex """.+/v\d.*-[a-z0-9]{7}/.+-.+/.+-.+-java-\d+\.\d+.*"""
  }
}
