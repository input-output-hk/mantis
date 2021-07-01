package io.iohk.ethereum.consensus.pow

import akka.util.ByteString

import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.crypto.kec256PoW
import io.iohk.ethereum.utils.ByteUtils

object KeccakCalculation {

  final val difficultyNumerator: BigInt = BigInt(2).pow(256)

  /** Computation of mixHash = keccak256(keccak256(rlp(unsealed header)), nonce)
    * @param hashHeader the rlp(unsealed header)
    * @return KeccakProofOWork containing the computed mixHash
    */
  def hash(hashHeader: Array[Byte], nonce: BigInt): KeccakMixHash = {
    val preHash = ByteString(kec256(hashHeader)).toArray
    val nonceBytes = ByteUtils.bigIntToUnsignedByteArray(nonce)
    val mixHash = kec256PoW(preHash, nonceBytes)

    KeccakMixHash(mixHash = ByteString(mixHash))
  }

  /** Validates if mixHash <= 2^256 / difficulty
    * @param mixHash
    * @param difficulty
    * @return boolean indicating whether PoW is valid or not
    */
  def isMixHashValid(mixHash: ByteString, difficulty: BigInt): Boolean = {
    val mixHashInt = BigInt.apply(mixHash.toArray)
    val threshold = difficultyNumerator / difficulty
    mixHashInt <= threshold
  }

  final case class KeccakMixHash(mixHash: ByteString)
}
