package io.iohk.ethereum.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VersionInfoSpec extends AnyFlatSpec with Matchers {
  behavior of "nodeName"

  it should "preserve major and minor Java version" in {
    VersionInfo.nodeName() should fullyMatch regex """mantis/v\d.*-[a-z0-9]{7}/.+-.+/.+-.+-java-\d+\.\d+.*"""
  }

  it should "augment the name with an identity" in {
    VersionInfo.nodeName(Some("etc")) should startWith("mantis-etc/")
  }
}
