package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.storage.ReferenceCountNodeStorage.{PruneCandidates, StoredNode}
import io.iohk.ethereum.rlp.RLPImplicitConversions.{toRlpList, _}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}

package object encoding {

  private[storage] def storedNodeFromBytes(encoded: Array[Byte]): StoredNode = decode(encoded)(storedNodeEncDec)

  private[storage] def pruneCandidatesFromBytes(array: Array[Byte]): PruneCandidates = decode(array)(pruneCandidatesEncDec)

  private[storage] def storedNodeToBytes(storedNode: StoredNode): Array[Byte] = storedNodeEncDec.encode(storedNode)

  private[storage] def pruneCandiatesToBytes(pruneCandidates: PruneCandidates): Array[Byte] = pruneCandidatesEncDec.encode(pruneCandidates)

  private val storedNodeEncDec = new RLPDecoder[StoredNode] with RLPEncoder[StoredNode] {
    override def decode(rlp: RLPEncodeable): StoredNode = rlp match {
      case RLPList(nodeEncoded, references) => StoredNode(nodeEncoded, references)
      case _ => throw new RuntimeException("Error when decoding stored node")
    }

    override def encode(obj: StoredNode): RLPEncodeable = rlpEncode(RLPList(obj.nodeEncoded, obj.references))
  }

  private val pruneCandidatesEncDec = new RLPEncoder[PruneCandidates] with RLPDecoder[PruneCandidates] {
    override def encode(obj: PruneCandidates): RLPEncodeable = rlpEncode(toRlpList(obj.nodeKeys))

    override def decode(rlp: RLPEncodeable): PruneCandidates = rlp match {
      case RLPList(candidates@_*) => PruneCandidates(candidates.map(b => b: ByteString))
      case _ => throw new RuntimeException("Error when decoding pruning candidate")
    }
  }
}
