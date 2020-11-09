package io.iohk.ethereum.proof

import akka.util.ByteString

trait NodeLoader {
  def getNode(hash: BigInt): Option[ByteString]
}

// TODO is this related to MptStorage ?
class StoredMerklePatriciaTrie[K, V](
    nodeLoader: NodeLoader,
    rootHash: BigInt,
    valueSerializer: V => ByteString,
    valueDeserializer: ByteString => V
) {
  // TODO besu implement StoredMerklePatriciaTrie as subclass of MPT
  // but our MerklePatriciaTrie has MPTStorage
}
