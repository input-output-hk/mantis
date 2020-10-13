package io.iohk.ethereum.utils

import com.typesafe.config.ConfigFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConfigUtilsSpec extends AnyWordSpec with Matchers {
  "keys" should {
    "return top-level keys of given config instance" in {
      val config = ConfigFactory.parseString("""{
        foo {
          bar {
            a = a
          }
        }
        baz = baz
      }""")

      ConfigUtils.keys(config) shouldBe Set("foo", "baz")
    }
  }
}
