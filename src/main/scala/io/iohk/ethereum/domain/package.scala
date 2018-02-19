package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.mpt.{ByteArrayEncoder, ByteArraySerializable, HashByteArraySerializable, MerklePatriciaTrie, NodesKeyValueStorage}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.utils.ByteUtils
import org.spongycastle.util.BigIntegers

package object domain {

  val byteArrayBigIntSerializer = new ByteArrayEncoder[BigInt] {
    override def toBytes(input: BigInt): Array[Byte] =
      ByteUtils.padLeft(ByteString(BigIntegers.asUnsignedByteArray(input.bigInteger)), 32).toArray[Byte]
  }

  val rlpBigIntSerializer = new ByteArraySerializable[BigInt] {
    override def fromBytes(bytes: Array[Byte]): BigInt = {
      BigIntegers.fromUnsignedByteArray(rlp.decode[ByteString](bytes).toArray[Byte])
    }

    override def toBytes(input: BigInt): Array[Byte] = {
      val enc = BigIntegers.asUnsignedByteArray(input.bigInteger)
      rlp.encode[ByteString](if (enc.isEmpty) ByteString(0x00) else ByteString(enc))
    }
  }

  def storageMpt(rootHash: ByteString, nodeStorage: NodesKeyValueStorage): MerklePatriciaTrie[BigInt, BigInt] =
    MerklePatriciaTrie[BigInt, BigInt](rootHash.toArray[Byte], nodeStorage)(HashByteArraySerializable(byteArrayBigIntSerializer), rlpBigIntSerializer)

}
