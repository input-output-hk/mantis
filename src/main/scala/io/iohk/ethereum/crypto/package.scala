package io.iohk.ethereum

import java.security.SecureRandom

import fr.cryptohash.Keccak256
import org.spongycastle.asn1.sec.SECNamedCurves
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECKeyGenerationParameters, ECDomainParameters}

package object crypto {

  val curveParams = SECNamedCurves.getByName("secp256k1")
  val curve = new ECDomainParameters(curveParams.getCurve, curveParams.getG, curveParams.getN, curveParams.getH)

  def sha3(input: Array[Byte], start: Int, length: Int): Array[Byte] = {
    val digest = new Keccak256
    digest.update(input, start, length)
    digest.digest
  }

  def sha3(input1: Array[Byte], input2: Array[Byte]): Array[Byte] = {
    val digest: Keccak256 = new Keccak256
    digest.update(input1, 0, input1.length)
    digest.update(input2, 0, input2.length)
    digest.digest
  }

  def sha3(input: Array[Byte]): Array[Byte] = {
    val digest: Keccak256 = new Keccak256
    digest.update(input)
    digest.digest
  }

  def generateKeyPair(): AsymmetricCipherKeyPair = {
    val generator = new ECKeyPairGenerator
    generator.init(new ECKeyGenerationParameters(curve, new SecureRandom))
    generator.generateKeyPair()
  }

}
