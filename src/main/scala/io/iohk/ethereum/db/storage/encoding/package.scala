package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.storage.ReferenceCountNodeStorage.{StoredNode, StoredNodeSnapshot}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}

package object encoding {

  private[storage] def snapshotsCountFromBytes(encoded: Array[Byte]): BigInt = decode(encoded)(bigIntEncDec)

  private[storage] def storedNodeFromBytes(encoded: Array[Byte]): StoredNode = decode(encoded)(storedNodeEncDec)

  private[storage] def snapshotFromBytes(encoded: Array[Byte]): StoredNodeSnapshot = decode(encoded)(snapshotEncDec)

  private[storage] def snapshotsCountToBytes(value: BigInt): Array[Byte] = rlpEncode(value)(bigIntEncDec)

  private[storage] def storedNodeToBytes(storedNode: StoredNode): Array[Byte] = rlpEncode(storedNodeEncDec.encode(storedNode))

  private[storage] def snapshotToBytes(snapshot: StoredNodeSnapshot): Array[Byte] = rlpEncode(snapshotEncDec.encode(snapshot))

  private val storedNodeEncDec = new RLPDecoder[StoredNode] with RLPEncoder[StoredNode] {
    override def decode(rlp: RLPEncodeable): StoredNode = rlp match {
      case RLPList(nodeEncoded, references, lastUsedByBlock) => StoredNode(nodeEncoded, references, lastUsedByBlock)
      case _ => throw new RuntimeException("Error when decoding stored node")
    }

    override def encode(obj: StoredNode): RLPEncodeable = RLPList(obj.nodeEncoded, obj.references, obj.lastUsedByBlock)
  }

  private val snapshotEncDec = new RLPDecoder[StoredNodeSnapshot] with RLPEncoder[StoredNodeSnapshot] {
    override def decode(rlp: RLPEncodeable): StoredNodeSnapshot = rlp match {
      case RLPList(nodeHash, storedNode) => StoredNodeSnapshot(byteStringFromEncodeable(nodeHash), Some(storedNodeFromBytes(storedNode)))
      case RLPValue(nodeHash) => StoredNodeSnapshot(byteStringFromEncodeable(nodeHash), None)
      case _ => throw new RuntimeException("Error when decoding stored nodes")
    }

    override def encode(objs: StoredNodeSnapshot): RLPEncodeable = objs match {
      case StoredNodeSnapshot(nodeHash, Some(storedNode)) => RLPList(byteStringToEncodeable(nodeHash), storedNodeToBytes(storedNode))
      case StoredNodeSnapshot(nodeHash, None) => RLPValue(byteStringToEncodeable(nodeHash))
    }
  }
}
