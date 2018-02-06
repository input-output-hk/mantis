package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.mpt.{ByteArraySerializable, HashByteArraySerializable, MerklePatriciaTrie, NodesKeyValueStorage}
import io.iohk.ethereum.rlp.RLPImplicits._

package object domain {

  val rlpByteStringSerializer = new ByteArraySerializable[ByteString] {
    override def fromBytes(bytes: Array[Byte]): ByteString = rlp.decode[ByteString](bytes)
    override def toBytes(input: ByteString): Array[Byte] = rlp.encode(if (input.forall(_ == 0x00)) ByteString(0x00) else input.dropWhile(_ == 0x00))
  }

  def storageMpt(rootHash: ByteString, nodeStorage: NodesKeyValueStorage): MerklePatriciaTrie[ByteString, ByteString] = {
    MerklePatriciaTrie[ByteString, ByteString](rootHash.toArray[Byte], nodeStorage)(
      HashByteArraySerializable(mpt.byteStringSerializer), rlpByteStringSerializer)
  }

}
