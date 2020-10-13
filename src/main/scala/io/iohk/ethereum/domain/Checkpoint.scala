package io.iohk.ethereum.domain

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.rlp._

case class Checkpoint(signatures: Seq[ECDSASignature])

object Checkpoint {

  import io.iohk.ethereum.crypto.ECDSASignatureImplicits._

  implicit val checkpointRLPEncoder: RLPEncoder[Checkpoint] = { checkpoint =>
    RLPList(checkpoint.signatures.map(_.toRLPEncodable): _*)
  }

  implicit val checkpointRLPDecoder: RLPDecoder[Checkpoint] = {
    case signatures: RLPList =>
      Checkpoint(
        signatures.items.map(ecdsaSignatureDec.decode)
      )
    case _ => throw new RuntimeException("Cannot decode Checkpoint")
  }

  def empty: Checkpoint = Checkpoint(Nil)
}
