package io.iohk.ethereum.db.cache

import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}

import scala.collection.mutable

trait MapCaches extends CacheComponent {
  private val nodeMap = mutable.Map.empty[NodeHash, Option[NodeEncoded]]

  val caches = new Caches {
    override val nodeCache: Cache[NodeHash, NodeEncoded] = new MapCache[NodeHash, NodeEncoded](nodeMap)
  }

}
