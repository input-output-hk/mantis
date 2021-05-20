package io.iohk.ethereum.consensus.pow

import akka.util.ByteString
import io.iohk.ethereum.consensus.pow.KeccakCalculation.KeccakMixHash
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.ByteUtils
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class KeccakCalculationSpec extends AnyFlatSpecLike with Matchers {
  import KeccakDataUtils._

  "KeccakCalculation" should
    // using the same data used in Besu test to prove that the same hash is generated
    "calculate the mixhash based on header hash and a nonce (Besu example)" in {
      val nonce = BigInt(-989241412479165825L) // BigInt(56)
      val nonceBytes = ByteUtils.bigIntToUnsignedByteArray(nonce)
      val rlpHeader = BlockHeader.getEncodedWithoutNonce(header)
      val mixhash = KeccakCalculation.hash(rlpHeader, nonceBytes)

      assert(
        ByteString(mixhash.mixHash) == ByteString(
          Hex.decode("d033f82e170ff16640e902fad569243c39bce9e4da948ccc298c541b34cd263b")
        )
      )
    }

  it should "compare the mixhash with the block difficulty (ECIP-1049 example)" in {
    val hash =
      KeccakMixHash(ByteString(Hex.decode("0116ad248e0dc3f7f843f73a62803c5f6b7c0427700b70c8b1aab39db404089f")))
    val blockDifficulty = 100L
    val isPoWValid = KeccakCalculation.isMixHashValid(hash.mixHash, blockDifficulty)

    assert(isPoWValid)
  }
}
