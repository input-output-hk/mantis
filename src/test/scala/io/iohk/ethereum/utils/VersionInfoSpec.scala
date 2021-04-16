package io.iohk.ethereum.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VersionInfoSpec extends AnyFlatSpec with Matchers {
  behavior of "nodeName"

  it should "match ethstats expected structure and preserve major and minor Java version" in {
    VersionInfo
      .nodeName() should fullyMatch regex """mantis/v\d(\.\d+)*(-SNAPSHOT)?-[a-z0-9]{7}/[^/]+-[^/]+/[^/]+-.[^/]+-java-\d+\.\d+[._0-9]*"""
  }

  it should "augment the name with an identity" in {
    val name = VersionInfo.nodeName(Some("iohk"))
    name should startWith("mantis/iohk/v")
    name.count(_ == '/') shouldBe 4
  }
}
