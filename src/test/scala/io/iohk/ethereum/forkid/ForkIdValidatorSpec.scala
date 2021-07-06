package io.iohk.ethereum.forkid

import akka.util.ByteString

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration._

import org.bouncycastle.util.encoders.Hex
import org.scalatest.matchers.should._
import org.scalatest.wordspec.AnyWordSpec

import io.iohk.ethereum.utils.Config._

import ForkIdValidator._

class ForkIdValidatorSpec extends AnyWordSpec with Matchers {

  val config = blockchains

  val ethGenesisHash: ByteString = ByteString(
    Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3")
  )

  "ForkIdValidator" must {
    "correctly validate ETH peers" in {
      // latest fork at the time of writing those assertions (in the spec) was Petersburg
      val ethForksList: List[BigInt] = List(1150000, 1920000, 2463000, 2675000, 4370000, 7280000)

      def validatePeer(head: BigInt, remoteForkId: ForkId) =
        ForkIdValidator
          .validatePeer[Task](ethGenesisHash, ethForksList)(head, remoteForkId)
          .runSyncUnsafe(Duration(1, SECONDS))

      // Local is mainnet Petersburg, remote announces the same. No future fork is announced.
      validatePeer(7987396, ForkId(0x668db0afL, None)) shouldBe Connect

      // Local is mainnet Petersburg, remote announces the same. Remote also announces a next fork
      // at block 0xffffffff, but that is uncertain.
      validatePeer(7279999, ForkId(0xa00bc324L, Some(ForkIdValidator.maxUInt64))) shouldBe Connect

      // Local is mainnet currently in Byzantium only (so it's aware of Petersburg), remote announces
      // also Byzantium, and it's also aware of Petersburg (e.g. updated node before the fork). We
      // don't know if Petersburg passed yet (will pass) or not.
      validatePeer(7279999, ForkId(0xa00bc324L, Some(7280000))) shouldBe Connect

      // Local is mainnet Petersburg, remote announces the same. Remote also announces a next fork
      // at block 0xffffffff, but that is uncertain.
      validatePeer(7987396, ForkId(0x668db0afL, Some(ForkIdValidator.maxUInt64))) shouldBe Connect

      // Local is mainnet currently in Byzantium only (so it's aware of Petersburg), remote announces
      // also Byzantium, but it's not yet aware of Petersburg (e.g. non updated node before the fork).
      // In this case we don't know if Petersburg passed yet or not.
      validatePeer(7279999, ForkId(0xa00bc324L, None)) shouldBe Connect

      validatePeer(7279999, ForkId(0xa00bc324L, Some(7280000))) shouldBe Connect

      // Local is mainnet currently in Byzantium only (so it's aware of Petersburg), remote announces
      // also Byzantium, and it's also aware of some random fork (e.g. misconfigured Petersburg). As
      // neither forks passed at neither nodes, they may mismatch, but we still connect for now.
      validatePeer(7279999, ForkId(0xa00bc324L, Some(ForkIdValidator.maxUInt64))) shouldBe Connect

      // Local is mainnet Petersburg, remote announces Byzantium + knowledge about Petersburg. Remote
      // is simply out of sync, accept.
      validatePeer(7987396, ForkId(0xa00bc324L, Some(7280000))) shouldBe Connect

      // Local is mainnet Petersburg, remote announces Spurious + knowledge about Byzantium. Remote
      // is definitely out of sync. It may or may not need the Petersburg update, we don't know yet.
      validatePeer(7987396, ForkId(0x3edd5b10L, Some(4370000))) shouldBe Connect

      // Local is mainnet Byzantium, remote announces Petersburg. Local is out of sync, accept.
      validatePeer(7279999, ForkId(0x668db0afL, None)) shouldBe Connect

      // Local is mainnet Spurious, remote announces Byzantium, but is not aware of Petersburg. Local
      // out of sync. Local also knows about a future fork, but that is uncertain yet.
      validatePeer(4369999, ForkId(0xa00bc324L, None)) shouldBe Connect

      // Local is mainnet Petersburg. remote announces Byzantium but is not aware of further forks.
      // Remote needs software update.
      validatePeer(7987396, ForkId(0xa00bc324L, None)) shouldBe ErrRemoteStale

      // Local is mainnet Petersburg, and isn't aware of more forks. Remote announces Petersburg +
      // 0xffffffff. Local needs software update, reject.
      validatePeer(7987396, ForkId(0x5cddc0e1L, None)) shouldBe ErrLocalIncompatibleOrStale

      // Local is mainnet Byzantium, and is aware of Petersburg. Remote announces Petersburg +
      // 0xffffffff. Local needs software update, reject.
      validatePeer(7279999, ForkId(0x5cddc0e1L, None)) shouldBe ErrLocalIncompatibleOrStale

      // Local is mainnet Petersburg, remote is Rinkeby Petersburg.
      validatePeer(7987396, ForkId(0xafec6b27L, None)) shouldBe ErrLocalIncompatibleOrStale

      // Local is mainnet Petersburg, far in the future. Remote announces Gopherium (non existing fork)
      // at some future block 88888888, for itself, but past block for local. Local is incompatible.
      //
      // This case detects non-upgraded nodes with majority hash power (typical Ropsten mess).
      validatePeer(88888888, ForkId(0x668db0afL, Some(88888888))) shouldBe ErrLocalIncompatibleOrStale

      // Local is mainnet Byzantium. Remote is also in Byzantium, but announces Gopherium (non existing
      // fork) at block 7279999, before Petersburg. Local is incompatible.
      validatePeer(7279999, ForkId(0xa00bc324L, Some(7279999L))) shouldBe ErrLocalIncompatibleOrStale
    }
  }
}
