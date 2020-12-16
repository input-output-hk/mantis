package io.iohk.ethereum.crypto

import akka.util.ByteString

object ECDSASignatureImplicits {

  import io.iohk.ethereum.rlp.RLPImplicitConversions._
  import io.iohk.ethereum.rlp.RLPImplicits._
  import io.iohk.ethereum.rlp._

  implicit val ecdsaSignatureDec: RLPDecoder[ECDSASignature] = new RLPDecoder[ECDSASignature] {
    override def decode(rlp: RLPEncodeable): ECDSASignature = rlp match {
      case RLPList(r, s, v) => ECDSASignature(r: ByteString, s: ByteString, v)
      case _ => throw new RuntimeException("Cannot decode ECDSASignature")
    }
  }

  implicit class ECDSASignatureEnc(ecdsaSignature: ECDSASignature) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = {
      RLPList(ecdsaSignature.r, ecdsaSignature.s, ecdsaSignature.v)
    }
  }

  implicit val ECDSASignatureOrdering: Ordering[ECDSASignature] = Ordering.by(sig => (sig.r, sig.s, sig.v))
}
