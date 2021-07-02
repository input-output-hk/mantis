package io.iohk.ethereum.consensus.pow.validators

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.consensus.pow.KeccakDataUtils
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderPoWError
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain.BlockHeader

class KeccakBlockHeaderValidatorSpec extends AnyFlatSpecLike with Matchers {
  import KeccakBlockHeaderValidatorSpec._

  "KeccakBlockHeaderValidatorSpec" should "return BlockHeaderValid when header is valid" in {
    KeccakBlockHeaderValidator.validateHeader(validBlockHeader) shouldBe Right(BlockHeaderValid)
  }

  it should "return HeaderPoWError when header is invalid" in {
    val invalidBlockHeader = validBlockHeader.copy(nonce = ByteString(Hex.decode("f245822d3413ab67")))
    KeccakBlockHeaderValidator.validateHeader(invalidBlockHeader) shouldBe Left(HeaderPoWError)
  }
}

object KeccakBlockHeaderValidatorSpec {
  import KeccakDataUtils._

  val validBlockHeader: BlockHeader = header.copy(
    mixHash = ByteString(Hex.decode("d033f82e170ff16640e902fad569243c39bce9e4da948ccc298c541b34cd263b")),
    nonce = ByteString(Hex.decode("f245822d3412da7f"))
  )
}
