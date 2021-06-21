package io.iohk.ethereum.forkid

import akka.util.ByteString
import io.iohk.ethereum.forkid.ForkId._
import io.iohk.ethereum.utils.ForkBlockNumbers
import io.iohk.ethereum.utils.Config._

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should._
import org.bouncycastle.util.encoders.Hex

import io.iohk.ethereum.rlp._
import io.iohk.ethereum.rlp.RLPImplicits._


class ForkIdSpec extends AnyWordSpec with Matchers {

  val config = blockchains

  "ForkId" must {
    "gatherForks for all chain configurations without errors" in {
      config.blockchains.map { case (name, conf) => (name, gatherForks(conf)) }
    }
    "gatherForks for the etc chain correctly" in {
      val res = config.blockchains.map { case (name, conf) => (name, gatherForks(conf)) }
      res("etc") shouldBe List(1150000, 2500000, 3000000, 5000000, 5900000, 8772000, 9573000, 10500839, 11700000)
    }

    "gatherForks for the eth chain correctly" in {
      val res = config.blockchains.map { case (name, conf) => (name, gatherForks(conf)) }
      res("eth") shouldBe List(1150000, 1920000, 2463000, 2675000, 4370000, 7280000, 9069000)
    }

    "create correct ForkId for ETH mainnet blocks" in {
      val ethConf = config.blockchains("eth")
      val ethGenesisHash = ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"))
      def create(head: BigInt) = ForkId.create(ethGenesisHash, ethConf)(head)

      create(0) shouldBe ForkId(0xfc64ec04L, Some(1150000)) // Unsynced
      create(1149999) shouldBe ForkId(0xfc64ec04L, Some(1150000)) // Last Frontier block
      create(1150000) shouldBe ForkId(0x97c2c34cL, Some(1920000)) // First Homestead block
      create(1919999) shouldBe ForkId(0x97c2c34cL, Some(1920000)) // Last Homestead block
      create(1920000) shouldBe ForkId(0x91d1f948L, Some(2463000)) // First DAO block
      create(2462999) shouldBe ForkId(0x91d1f948L, Some(2463000)) // Last DAO block
      create(2463000) shouldBe ForkId(0x7a64da13L, Some(2675000)) // First Tangerine block
      create(2674999) shouldBe ForkId(0x7a64da13L, Some(2675000)) // Last Tangerine block
      create(2675000) shouldBe ForkId(0x3edd5b10L, Some(4370000)) // First Spurious block
      create(4369999) shouldBe ForkId(0x3edd5b10L, Some(4370000)) // Last Spurious block
      create(4370000) shouldBe ForkId(0xa00bc324L, Some(7280000)) // First Byzantium block
      create(7279999) shouldBe ForkId(0xa00bc324L, Some(7280000)) // Last Byzantium block
      create(7280000) shouldBe ForkId(0x668db0afL, Some(9069000)) // First and last Constantinople, first Petersburg block
      create(9068999) shouldBe ForkId(0x668db0afL, Some(9069000)) // Last Petersburg block
      // TODO: Add Muir Glacier and Berlin
      create(9069000) shouldBe ForkId(0x879d6e30L, None) // First Istanbul block
      create(12644529) shouldBe ForkId(0x879d6e30L, None) // Today Istanbul block
    }

    "create correct ForkId for ETC mainnet blocks" in {
      val etcConf = config.blockchains("etc")
      val etcGenesisHash = ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"))
      def create(head: BigInt) = ForkId.create(etcGenesisHash, etcConf)(head)

      create(0) shouldBe ForkId(0xfc64ec04L, Some(1150000)) // Unsynced
      create(1149999) shouldBe ForkId(0xfc64ec04L, Some(1150000)) // Last Frontier block
      create(1150000) shouldBe ForkId(0x97c2c34cL, Some(2500000)) // First Homestead block
      create(1919999) shouldBe ForkId(0x97c2c34cL, Some(2500000)) // Last Homestead block
      create(2500000) shouldBe ForkId(0xdb06803fL, Some(3000000))
      create(3000000-1) shouldBe ForkId(0xdb06803fL, Some(3000000))
      create(3000000) shouldBe ForkId(0xaff4bed4L, Some(5000000))
      create(5000000-1) shouldBe ForkId(0xaff4bed4L, Some(5000000))
      create(5000000) shouldBe ForkId(0xf79a63c0L, Some(5900000))
      create(5900000-1) shouldBe ForkId(0xf79a63c0L, Some(5900000))
      create(5900000) shouldBe ForkId(0x744899d6L, Some(8772000))
      create(8772000-1) shouldBe ForkId(0x744899d6L, Some(8772000))
      create(8772000) shouldBe ForkId(0x518b59c6L, Some(9573000))
      create(9573000-1) shouldBe ForkId(0x518b59c6L, Some(9573000))
      create(9573000) shouldBe ForkId(0x7ba22882L, Some(10500839))
      create(10500839-1) shouldBe ForkId(0x7ba22882L, Some(10500839))
      create(10500839) shouldBe ForkId(0x9007bfccL, Some(11700000))
      create(11700000-1) shouldBe ForkId(0x9007bfccL, Some(11700000))
      create(11700000) shouldBe ForkId(0xdb63a1caL, None)
    }

    // Here’s a couple of tests to verify the proper RLP encoding (since FORK_HASH is a 4 byte binary but FORK_NEXT is an 8 byte quantity):
    "be correctly encoded via rlp" in {
      roundTrip(ForkId(0, None), "c6840000000080")
      roundTrip(ForkId(0xdeadbeefL, Some(0xBADDCAFEL)), "ca84deadbeef84baddcafe")

      val maxUInt64 = (BigInt(0x7FFFFFFFFFFFFFFFL) << 1) + 1
      maxUInt64.toByteArray shouldBe Array(0, -1, -1, -1, -1, -1, -1, -1, -1)
      val maxUInt32 = BigInt(0xFFFFFFFFL)
      maxUInt32.toByteArray shouldBe Array(0, -1, -1, -1, -1)

      roundTrip(ForkId(maxUInt32, Some(maxUInt64)), "ce84ffffffff88ffffffffffffffff")
    }
  }

  private def roundTrip(forkId: ForkId, hex: String) = {
    encode(forkId.toRLPEncodable) shouldBe Hex.decode(hex)
    decode[ForkId](Hex.decode(hex)) shouldBe forkId
  }
}
