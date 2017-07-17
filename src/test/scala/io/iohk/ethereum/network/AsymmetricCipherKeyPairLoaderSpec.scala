package io.iohk.ethereum.network

import java.io.File
import java.nio.file.Files

import io.iohk.ethereum.network
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}

class AsymmetricCipherKeyPairLoaderSpec extends FlatSpec with Matchers with SecureRandomBuilder {

  def withFilePath(testCode: String => Any): Unit = {
    val path = Files.createTempFile("key-", "").toAbsolutePath.toString
    require(new File(path).delete(), "File deletion before test failed")
    try {
      testCode(path)
    } finally {
      val file = new File(path)
      assert(!file.exists() || file.delete(), "File deletion after test failed")
    }
  }

  def equalKeyPairs(keyPair1: AsymmetricCipherKeyPair, keyPair2: AsymmetricCipherKeyPair): Boolean = {
    //Compare public keys
    val publicKeyParam1 = keyPair1.getPublic.asInstanceOf[ECPublicKeyParameters]
    val publicKeyParam2 = keyPair2.getPublic.asInstanceOf[ECPublicKeyParameters]
    val equalPublicKey =
      publicKeyParam1.getQ == publicKeyParam2.getQ &&
        publicKeyParam1.getParameters == publicKeyParam2.getParameters &&
        publicKeyParam1.isPrivate == publicKeyParam2.isPrivate

    //Compare private keys
    val privateKeyParam1 = keyPair1.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val privateKeyParam2 = keyPair2.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val equalPrivateKey =
      privateKeyParam1.getD == privateKeyParam2.getD &&
        privateKeyParam1.getParameters == privateKeyParam2.getParameters &&
        privateKeyParam1.isPrivate == privateKeyParam2.isPrivate

    equalPrivateKey && equalPublicKey
  }

  it should "correctly save the AsymmetricCipherKeyPairLoader" in {
    withFilePath { path =>
      //Create key pair
      val newKeyPair = network.loadAsymmetricCipherKeyPair(path, secureRandom)

      //Read key pair from file
      val obtainedKeyPair = network.loadAsymmetricCipherKeyPair(path, secureRandom)

      assert(equalKeyPairs(newKeyPair, obtainedKeyPair))
    }
  }
}
