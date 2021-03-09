package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.domain.{Account, Address}
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Span}

import java.time.Duration

class ExpiringMapSpec extends AnyFlatSpec with Matchers with Eventually {

  val testResolution = 20
  val holdTime = 100
  val holdTimeDur = Duration.ofMillis(holdTime)
  val waitTime = holdTime + testResolution

  implicit override val patienceConfig =
    PatienceConfig(timeout = scaled(Span(waitTime, Millis)), interval = scaled(Span(testResolution, Millis)))

  it should "Put element in, for correct amount of time and not retain it afterwards" in new TestSetup {
    expiringMap.add(address1, account1, holdTimeDur)

    expiringMap.get(address1) shouldEqual Some(account1)

    eventually {
      expiringMap.get(address1) shouldBe None
    }

    expiringMap.get(address1) shouldBe None
  }

  it should "Put element in, for negative duration (element will be inaccessible and removed at first occasion)" in new TestSetup {
    expiringMap.add(address1, account1, Duration.ofMillis(-50))

    expiringMap.get(address1) shouldEqual None
  }

  it should "Put new element in with new holdTime, for correct amount of time and not retain it afterwards" in new TestSetup {
    expiringMap.add(address1, account1, holdTimeDur)

    expiringMap.get(address1) shouldEqual Some(account1)

    eventually {
      expiringMap.get(address1) shouldBe None
    }

    expiringMap.get(address1) shouldBe None

    expiringMap.add(address1, account2, holdTimeDur)

    expiringMap.get(address1) shouldEqual Some(account2)

    eventually {
      expiringMap.get(address1) shouldBe None
    }

    expiringMap.get(address1) shouldBe None
  }

  it should "Put two elements in, for different amount of time and not retain it afterwards" in new TestSetup {
    expiringMap.add(address1, account1, holdTimeDur)
    expiringMap.add(address2, account2, holdTimeDur.plusMillis(50))

    expiringMap.get(address1) shouldEqual Some(account1)
    expiringMap.get(address2) shouldEqual Some(account2)

    eventually {
      expiringMap.get(address1) shouldBe None
      expiringMap.get(address2) shouldEqual Some(account2)
    }

    eventually {
      expiringMap.get(address2) shouldBe None
    }
  }

  it should "Put element in, for default amount of time and not retain it afterwards" in new TestSetup {
    expiringMap.add(address1, account1)

    expiringMap.get(address1) shouldEqual Some(account1)

    eventually {
      expiringMap.get(address1) shouldBe None
    }

    expiringMap.get(address1) shouldBe None
  }

  it should "Put element in, until some time and not retain it afterwards" in new TestSetup {
    expiringMap.addFor(address1, account1, holdTimeDur)

    expiringMap.get(address1) shouldEqual Some(account1)

    eventually {
      expiringMap.get(address1) shouldBe None
    }

    expiringMap.get(address1) shouldBe None
  }

  it should "not overflow and throw exception when adding duration with max seconds" in new TestSetup {
    expiringMap.add(address1, account1, Duration.ofSeconds(Long.MaxValue))

    expiringMap.get(address1) shouldEqual Some(account1)
  }

  it should "It should remove existing element" in new TestSetup {
    expiringMap.add(address1, account1, holdTimeDur)
    expiringMap.get(address1) shouldEqual Some(account1)
    expiringMap.remove(address1)
    expiringMap.get(address1) shouldBe None
  }

  it should "It should overwrite existing element" in new TestSetup {
    expiringMap.add(address1, account1, holdTimeDur)
    expiringMap.get(address1) shouldEqual Some(account1)
    expiringMap.add(address1, account2, holdTimeDur)
    expiringMap.get(address1) shouldBe Some(account2)
  }

  trait TestSetup {
    val defaultHoldTime = holdTime
    val expiringMap: ExpiringMap[Address, Account] = ExpiringMap.empty(Duration.ofMillis(defaultHoldTime))
    val address1 = Address(0x123456)
    val address2 = Address(0xabcdef)

    val account1 = Account.empty()
    val account2 = Account.empty().increaseBalance(10)
  }
}
