package io.iohk.ethereum.network

import java.math.BigInteger
import java.security.SecureRandom

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import org.scalatest.{Matchers, FlatSpec}
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECPublicKeyParameters, ECKeyGenerationParameters}
import org.spongycastle.util.encoders.Hex

import scala.util.Random

class AuthInitiateMessageSpec extends FlatSpec with Matchers {

  "AuthInitiateMessage" should "encode and decode itself" in {
    val keyPair = {
      val generator = new ECKeyPairGenerator
      generator.init(new ECKeyGenerationParameters(curve, new SecureRandom))
      generator.generateKeyPair()
    }

    val nonce = new Array[Byte](32)
    Random.nextBytes(nonce)

    val signature = ECDSASignature(new BigInteger("123"), new BigInteger("456"), 0)

    val msg = AuthInitiateMessage(signature, ByteString(Array.fill(32)(0.toByte)), keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ, ByteString(nonce), false)

    AuthInitiateMessage.decode(msg.encode()) shouldBe msg
  }

  it should "decode predefined message" in {
    val inputHex = "B50149D21491FD549AA7B60BF18B65865D078DC09A4BD10A574457FF1AB1C0193DAF513F7D48B7C2EAD41A7F10DD56B167D83E8F649A2795757CFE4965AA930E00F30BAC135324F493AEEAF4C9B48DF0F1F9A28A3DEDF9B1D85AF45A27F7B6F0546F8A80D14311C39F35F516FA664DEAAAA13E85B2F7493F37F6144D86991EC012937307647BD3B9A82ABE2974E1407241D54947BBB39763A4CAC9F77166AD92A0CAE0187FD4EB042EB7A47EEDDA185CFEB59DEA550418D1D036B76E03A5BF74AC00"

    val expectedMsg = AuthInitiateMessage(
      signature = ECDSASignature(
        r = new BigInteger("81870901931874412952660009205824222047471672340278145383000560930072854380569"),
        s = new BigInteger("27900842753040147848386185004093503271309114686926362065982995477587410195214"),
        v = 27),
      ephemeralPublicHash = ByteString(Hex.decode("F30BAC135324F493AEEAF4C9B48DF0F1F9A28A3DEDF9B1D85AF45A27F7B6F054")),
      publicKey = curve.getCurve.decodePoint(Hex.decode("046F8A80D14311C39F35F516FA664DEAAAA13E85B2F7493F37F6144D86991EC012937307647BD3B9A82ABE2974E1407241D54947BBB39763A4CAC9F77166AD92A0")),
      nonce = ByteString(Hex.decode("CAE0187FD4EB042EB7A47EEDDA185CFEB59DEA550418D1D036B76E03A5BF74AC")),
      knownPeer = false)

    val decoded = AuthInitiateMessage.decode(Hex.decode(inputHex))

    decoded shouldBe expectedMsg
  }

}
