package io.iohk.ethereum.checkpointing

import akka.util.ByteString

import org.bouncycastle.crypto.AsymmetricCipherKeyPair

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.crypto.ECDSASignatureImplicits.ECDSASignatureOrdering

object CheckpointingTestHelpers {

  def createCheckpointSignatures(
      keys: Seq[AsymmetricCipherKeyPair],
      hash: ByteString
  ): Seq[ECDSASignature] =
    keys.map { k =>
      ECDSASignature.sign(hash.toArray, k)
    }.sorted
}
