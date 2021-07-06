package io.iohk.ethereum.crypto

import akka.util.ByteString

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import io.iohk.ethereum.crypto
import io.iohk.ethereum.jsonrpc.JsonMethodsImplicits
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.ByteStringUtils

// scalastyle:off regex
object SignatureValidator extends App with SecureRandomBuilder with JsonMethodsImplicits {

  args match {
    case Array(pk, sig, msgHash) =>
      Try {
        val signature = ECDSASignature.fromBytes(ByteStringUtils.string2hash(sig))
        val msg = ByteStringUtils.string2hash(msgHash)

        signature.flatMap(_.publicKey(msg))
      } match {
        case Failure(exception) =>
          System.err.println(
            s"Can't recover public key from signature [$sig] and msg [$msgHash]: ERROR: ${exception.getMessage}"
          )
          sys.exit(1)
        case Success(recoveredPk) =>
          val publicKey = ByteStringUtils.string2hash(pk)
          recoveredPk match {
            case Some(pk) =>
              if (pk == publicKey) {
                System.err.println(
                  s"Recovered public key [${ByteStringUtils.hash2string(pk)}] is the same as given one"
                )
              } else {
                System.err.println(s"Recovered public key [${ByteStringUtils
                  .hash2string(pk)}] is different than given [${ByteStringUtils.hash2string(publicKey)}]")
                sys.exit(1)
              }
            case None =>
              System.err.println(s"Can't recover public key from signature [$sig] and msg [$msgHash]")
              sys.exit(1)
          }
      }
    case _ =>
      val keyPair = crypto.generateKeyPair(secureRandom)
      val pkStr = ByteStringUtils.hash2string(ByteString(crypto.pubKeyFromKeyPair(keyPair)))
      val hash = kec256(Array(1.toByte))
      val hashStr = ByteStringUtils.hash2string(ByteString(hash))
      val signature = ECDSASignature.sign(hash, keyPair)
      val sigStr = ByteStringUtils.hash2string(signature.toBytes)
      System.err.println(
        s"Bad Input. Example usage: [signature-validator publicKey signature message_hash]. Example: [signature-validator $pkStr $sigStr $hashStr]"
      )
      sys.exit(1)
  }
}
