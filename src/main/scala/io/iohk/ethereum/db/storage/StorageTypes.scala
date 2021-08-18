package io.iohk.ethereum.db.storage

import akka.util.ByteString

object StorageTypes {
  type BlockHash = ByteString
  type BlockHeaderHash = ByteString
  type BlockBodyHash = ByteString
  type NodeHash = ByteString
  type NodeEncoded = Array[Byte]
}
