package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.storage.NodeStorage.NodeHash
import io.iohk.ethereum.db.storage.ReferenceCountNodeStorage.StoredNode
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}

package object encoding {

  private[storage] def storedNodeFromBytes(encoded: Array[Byte]): StoredNode = decode(encoded)(storedNodeEncDec)

  private[storage] def storedNodesFromBytes(encoded: Array[Byte]): Seq[(NodeHash, StoredNode)] = decode(encoded)(storedNodesEncDec)

  private[storage] def storedNodeToBytes(storedNode: StoredNode): Array[Byte] = rlpEncode(storedNodeEncDec.encode(storedNode))

  private[storage] def storedNodesToBytes(storedNodes: Seq[(NodeHash, StoredNode)]): Array[Byte] = rlpEncode(storedNodesEncDec.encode(storedNodes))

  private val storedNodeEncDec = new RLPDecoder[StoredNode] with RLPEncoder[StoredNode] {
    override def decode(rlp: RLPEncodeable): StoredNode = rlp match {
      case RLPList(nodeEncoded, references) => StoredNode(nodeEncoded, references)
      case _ => throw new RuntimeException("Error when decoding stored node")
    }

    override def encode(obj: StoredNode): RLPEncodeable = RLPList(obj.nodeEncoded, obj.references)
  }

  private val storedNodesEncDec = new RLPDecoder[Seq[(NodeHash, StoredNode)]] with RLPEncoder[Seq[(NodeHash, StoredNode)]] {
    override def decode(rlp: RLPEncodeable) = rlp match {
      case rlpList: RLPList => rlpList.items.map {
        case RLPList(nodeHash, storedNode) => byteStringFromEncodeable(nodeHash) -> storedNodeEncDec.decode(storedNode)
        case _ => throw new RuntimeException("Error when decoding stored nodes")
      }
      case _ => throw new RuntimeException("Error when decoding stored nodes")
    }

    override def encode(objs: Seq[(NodeHash, StoredNode)]) = RLPList(objs.map(obj => RLPList(obj._1, storedNodeEncDec.encode(obj._2))):_*)
  }
}
