package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.db.storage.MptStorage
import io.iohk.ethereum.mpt.{ByteArrayEncoder, ByteArraySerializable, HashByteArraySerializable, MerklePatriciaTrie}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.utils.ByteUtils
import org.bouncycastle.util.BigIntegers

package object domain {
  type HeadersSeq = Seq[BlockHeader]

  object EthereumUInt256Mpt {
    val byteArrayBigIntSerializer = new ByteArrayEncoder[BigInt] {
      override def toBytes(input: BigInt): Array[Byte] =
        ByteUtils.padLeft(ByteString(BigIntegers.asUnsignedByteArray(input.bigInteger)), 32).toArray[Byte]
    }

    val rlpBigIntSerializer = new ByteArraySerializable[BigInt] {
      override def fromBytes(bytes: Array[Byte]): BigInt = rlp.decode[BigInt](bytes)

      override def toBytes(input: BigInt): Array[Byte] = rlp.encode[BigInt](input)
    }

    def storageMpt(rootHash: ByteString, nodeStorage: MptStorage): MerklePatriciaTrie[BigInt, BigInt] =
      MerklePatriciaTrie[BigInt, BigInt](rootHash.toArray[Byte], nodeStorage)(
        HashByteArraySerializable(byteArrayBigIntSerializer),
        rlpBigIntSerializer
      )
  }

  object ArbitraryIntegerMpt {
    val bigIntSerializer = new ByteArraySerializable[BigInt] {
      override def fromBytes(bytes: Array[Byte]): BigInt = BigInt(bytes)
      override def toBytes(input: BigInt): Array[Byte] = input.toByteArray
    }

    def storageMpt(rootHash: ByteString, nodeStorage: MptStorage): MerklePatriciaTrie[BigInt, BigInt] =
      MerklePatriciaTrie[BigInt, BigInt](rootHash.toArray[Byte], nodeStorage)(
        HashByteArraySerializable(bigIntSerializer),
        bigIntSerializer
      )
  }

}
