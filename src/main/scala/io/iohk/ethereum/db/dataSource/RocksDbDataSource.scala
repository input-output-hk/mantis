package io.iohk.ethereum.db.dataSource

import java.util.concurrent.locks.ReentrantReadWriteLock

import org.rocksdb._

class RocksDbDataSource(private var db: RocksDB, private val rocksDbConfig: RocksDbConfig) extends DataSource {

  /**
    * This lock is needed because close operation in RocksDb is not thread-safe.
    */
  private val dbLock = new ReentrantReadWriteLock()

  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key
    * @return the value associated with the passed key.
    */
  override def get(namespace: Namespace, key: Key): Option[Value] = Option(db.get((namespace ++ key).toArray))

  /**
    * This function obtains the associated value to a key, if there exists one. It assumes that
    * caller already properly serialized key. Useful when caller knows some pattern in data to
    * avoid generic serialization.
    *
    * @param key
    * @return the value associated with the passed key.
    */
  override def getOptimized(key: Array[Byte]): Option[Array[Byte]] = Option(db.get(key))

  /**
    * This function updates the DataSource by deleting, updating and inserting new (key-value) pairs.
    *
    * @param namespace from which the (key-value) pairs will be removed and inserted.
    * @param toRemove  which includes all the keys to be removed from the DataSource.
    * @param toUpsert  which includes all the (key-value) pairs to be inserted into the DataSource.
    *                  If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(namespace: Namespace, toRemove: Seq[Key], toUpsert: Seq[(Key, Value)]): DataSource = {
    val batch = new WriteBatch()
    toRemove.foreach { key => batch.delete((namespace ++ key).toArray) }
    toUpsert.foreach { item => batch.put((namespace ++ item._1).toArray, item._2.toArray) }
    db.write(new WriteOptions(), batch)
    this
  }

  /**
    * This function updates the DataSource by deleting, updating and inserting new (key-value) pairs.
    * It assumes that caller already properly serialized key and value.
    * Useful when caller knows some pattern in data to avoid generic serialization.
    *
    * @param toRemove which includes all the keys to be removed from the DataSource.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the DataSource.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def updateOptimized(toRemove: Seq[Array[Byte]], toUpsert: Seq[(Array[Byte], Array[Byte])]): DataSource = {
    val batch = new WriteBatch()
    toRemove.foreach { key => batch.delete(key) }
    toUpsert.foreach { item => batch.put(item._1, item._2) }
    db.write(new WriteOptions(), batch)
    this
  }

  /**
    * This function updates the DataSource by deleting all the (key-value) pairs in it.
    *
    * @return the new DataSource after all the data was removed.
    */
  override def clear: DataSource = {
    destroy()
    this.db = RocksDbDataSource.createDB(rocksDbConfig)
    this
  }

  /**
    * This function closes the DataSource, without deleting the files used by it.
    */
  override def close(): Unit = {
    dbLock.writeLock().lock()
    try { db.close() } finally { dbLock.writeLock().unlock() }
  }

  /**
    * This function closes the DataSource, if it is not yet closed, and deletes all the files used by it.
    */
  override def destroy(): Unit = {
    try {
      close()
    } finally {
      import rocksDbConfig._

      val options = new Options()
      options.setCreateIfMissing(createIfMissing)
      options.setParanoidChecks(paranoidChecks)

      org.rocksdb.RocksDB.destroyDB(path, options)
    }
  }
}

trait RocksDbConfig {
  val createIfMissing: Boolean
  val paranoidChecks: Boolean
  val path: String
}

object RocksDbDataSource {

  private def createDB(rocksDbConfig: RocksDbConfig): RocksDB = {
    import rocksDbConfig._

    val options = new Options()
    options.setCreateIfMissing(createIfMissing)
    options.setParanoidChecks(paranoidChecks)

    org.rocksdb.RocksDB.open(options, path)
  }

  def apply(rocksDbConfig: RocksDbConfig): RocksDbDataSource = {
    new RocksDbDataSource(createDB(rocksDbConfig), rocksDbConfig)
  }
}

