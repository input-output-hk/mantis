package io.iohk.ethereum.utils

import com.typesafe.config.ConfigFactory
import org.scalatest.{Matchers, WordSpec}

class ConfigUtilsSpec extends WordSpec with Matchers {
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
