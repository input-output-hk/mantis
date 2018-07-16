package io.iohk.ethereum.db.dataSource

import java.util.concurrent.locks.ReentrantReadWriteLock

import org.rocksdb._

class RocksDbDataSource(private var db: RocksDB, private val rocksDbConfig: RocksDbConfig) extends DataSource {

  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key       the key retrieve the value.
    * @return the value associated with the passed key.
    */
  override def get(namespace: Namespace, key: Key): Option[Value] = {

    val readOptions = new ReadOptions().setVerifyChecksums(rocksDbConfig.verifyChecksums)
    Option(db.get(readOptions, (namespace ++ key).toArray))

  }

  /**
    * This function obtains the associated value to a key, if there exists one. It assumes that
    * caller already properly serialized key. Useful when caller knows some pattern in data to
    * avoid generic serialization.
    *
    * @param key the key retrieve the value.
    * @return the value associated with the passed key.
    */
  override def getOptimized(key: Array[Byte]): Option[Array[Byte]] = {

    val readOptions = new ReadOptions().setVerifyChecksums(rocksDbConfig.verifyChecksums)
    Option(db.get(readOptions, key))

  }

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
    toRemove.foreach{ key => batch.delete((namespace ++ key).toArray) }
    toUpsert.foreach{ item => batch.put((namespace ++ item._1).toArray, item._2.toArray) }
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
    toRemove.foreach{ key => batch.delete(key) }
    toUpsert.foreach{ item => batch.put(item._1, item._2) }
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
    RocksDbDataSource.dbLock.writeLock().lock()
    try {
      db.close()
    } finally {
      RocksDbDataSource.dbLock.writeLock().unlock()
    }
  }

  /**
    * This function closes the DataSource, if it is not yet closed, and deletes all the files used by it.
    */
  override def destroy(): Unit = {
    close()

    import rocksDbConfig._

    val options = new Options()
      .setCreateIfMissing(createIfMissing)
      .setParanoidChecks(paranoidChecks)
      .setCompressionType(CompressionType.LZ4_COMPRESSION)
      .setBottommostCompressionType(CompressionType.ZSTD_COMPRESSION)
      .setLevelCompactionDynamicLevelBytes(true)
      .setMaxOpenFiles(maxOpenFiles)
      .setIncreaseParallelism(maxThreads)

    org.rocksdb.RocksDB.destroyDB(path, options)

  }
}

trait RocksDbConfig {
  val createIfMissing: Boolean
  val paranoidChecks: Boolean
  val path: String
  val maxThreads: Int
  val maxOpenFiles: Int
  val verifyChecksums: Boolean
}

object RocksDbDataSource {

  /**
    * This lock is needed because close and open operations in RocksDb are not thread-safe.
    */
  private val dbLock = new ReentrantReadWriteLock()

  private def createDB(rocksDbConfig: RocksDbConfig): RocksDB = {
    import rocksDbConfig._

    val options = new Options()
      .setCreateIfMissing(createIfMissing)
      .setParanoidChecks(paranoidChecks)
      .setCompressionType(CompressionType.LZ4_COMPRESSION)
      .setBottommostCompressionType(CompressionType.ZSTD_COMPRESSION)
      .setLevelCompactionDynamicLevelBytes(true)
      .setMaxOpenFiles(maxOpenFiles)
      .setIncreaseParallelism(maxThreads)

    RocksDB.loadLibrary()

    org.rocksdb.RocksDB.open(options, path)

  }

  def apply(rocksDbConfig: RocksDbConfig): RocksDbDataSource = {
    new RocksDbDataSource(createDB(rocksDbConfig), rocksDbConfig)
  }
}

