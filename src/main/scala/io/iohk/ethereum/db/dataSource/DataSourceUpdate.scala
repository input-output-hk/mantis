package io.iohk.ethereum.db.dataSource

import io.iohk.ethereum.db.dataSource.DataSource.{Key, Namespace, Value}

sealed trait DataUpdate

/**
  * This represent updates to be performed on the DataSource by deleting, updating and inserting new (key-value) pairs.
  *
  * @param namespace from which the (key-value) pairs will be removed and inserted.
  * @param toRemove which includes all the keys to be removed from the DataSource.
  * @param toUpsert which includes all the (key-value) pairs to be inserted into the DataSource.
  *                 If a key is already in the DataSource its value will be updated.
  */
case class DataSourceUpdate(namespace: Namespace, toRemove: Seq[Key], toUpsert: Seq[(Key, Value)]) extends DataUpdate

/**
  * This represent updates the DataSource by deleting, updating and inserting new (key-value) pairs.
  * It assumes that caller already properly serialized key and value.
  * Useful when caller knows some pattern in data to avoid generic serialization.
  *
  * @param toRemove which includes all the keys to be removed from the DataSource.
  * @param toUpsert which includes all the (key-value) pairs to be inserted into the DataSource.
  *                 If a key is already in the DataSource its value will be updated.
  */
case class DataSourceUpdateOptimized(
    namespace: Namespace,
    toRemove: Seq[Array[Byte]],
    toUpsert: Seq[(Array[Byte], Array[Byte])]
) extends DataUpdate
