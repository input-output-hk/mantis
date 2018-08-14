package io.iohk.ethereum.db.dataSource


import java.util.concurrent.locks.ReentrantReadWriteLock
import io.iohk.ethereum.db.dataSource.DataSource._
import io.iohk.ethereum.utils.TryWithResources.withResources
import org.rocksdb._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.util.control.NonFatal

class RocksDbDataSource(
  private var db: RocksDB,
  private val rocksDbConfig: RocksDbConfig,
  private var readOptions: ReadOptions,
  private var dbOptions: DBOptions,
  private var cfOptions: ColumnFamilyOptions,
  private var nameSpaces: Seq[Namespace],
  private var handles: Map[Namespace, ColumnFamilyHandle]
) extends DataSource {

  private val logger = LoggerFactory.getLogger("rocks-db")

  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key       the key retrieve the value.
    * @return the value associated with the passed key.
    */
  override def get(namespace: Namespace, key: Key): Option[Value] = {
    RocksDbDataSource.dbLock.readLock().lock()
    try {
      Option(db.get(handles(namespace), readOptions, key.toArray))
    } catch {
      case NonFatal(e) =>
        logger.error(s"Not found associated value to a namespace: $namespace and a key: $key, cause: {}", e.getMessage)
        throw new RuntimeException(e)
    } finally {
      RocksDbDataSource.dbLock.readLock().unlock()
    }
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
    RocksDbDataSource.dbLock.readLock().lock()
    try {
      Option(db.get(readOptions, key))
    } catch {
      case NonFatal(e) =>
        logger.error(s"Not found associated value to a key: $key, cause: {}", e.getMessage)
        throw new RuntimeException(e)
    } finally {
      RocksDbDataSource.dbLock.readLock().unlock()
    }
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
    RocksDbDataSource.dbLock.readLock().lock()
    try {
      withResources(new WriteOptions()){ writeOptions =>
        withResources(new WriteBatch()){ batch =>
          toRemove.foreach{ key => batch.delete(handles(namespace), key.toArray) }
          toUpsert.foreach{ case (k, v) => batch.put(handles(namespace),  k.toArray, v.toArray) }

          db.write(writeOptions, batch)
        }
      }
    } catch {
      case NonFatal(e) =>
        logger.error(s"DataSource not updated (toRemove: ${ toRemove.size }, toUpsert: ${ toUpsert.size }, namespace: $namespace), cause: {}", e.getMessage)
        throw new RuntimeException(e)
    } finally {
      RocksDbDataSource.dbLock.readLock().unlock()
    }
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
    RocksDbDataSource.dbLock.readLock().lock()
    try {
      withResources(new WriteOptions()){ writeOptions =>
        withResources(new WriteBatch()){ batch =>
          toRemove.foreach{ key => batch.delete(key) }
          toUpsert.foreach{ case (k, v) => batch.put(k, v) }

          db.write(writeOptions, batch)
        }
      }
    } catch {
      case NonFatal(e) =>
        logger.error(s"DataSource not updated (toRemove: ${ toRemove.size }, toUpsert: ${ toUpsert.size }), cause: {}", e.getMessage)
        throw new RuntimeException(e)
    } finally {
      RocksDbDataSource.dbLock.readLock().unlock()
    }
    this
  }

  /**
    * This function updates the DataSource by deleting all the (key-value) pairs in it.
    *
    * @return the new DataSource after all the data was removed.
    */
  override def clear: DataSource = {
    destroy()
    logger.debug(s"About to create new DataSource for path: ${ rocksDbConfig.path }")
    val (newDb, handles, readOptions, dbOptions, cfOptions) = RocksDbDataSource.createDB(rocksDbConfig, nameSpaces)
    this.db = newDb
    this.readOptions = readOptions
    this.handles = nameSpaces.zip(handles).toMap
    this.dbOptions = dbOptions
    this.cfOptions = cfOptions
    this
  }

  /**
    * This function closes the DataSource, without deleting the files used by it.
    */
  override def close(): Unit = {
    logger.debug(s"About to close DataSource in path: ${ rocksDbConfig.path }")
    RocksDbDataSource.dbLock.writeLock().lock()
    try {
      // There is specific order for closing rocksdb with column families descibed in
      // https://github.com/facebook/rocksdb/wiki/RocksJava-Basics#opening-a-database-with-column-families
      // 1. Free all column families handles
      handles.values.foreach(_.close())
      // 2. Free db and db options
      db.close()
      readOptions.close()
      dbOptions.close()
      // 3. Free column families options
      cfOptions.close()
    } catch {
      case NonFatal(e) =>
        logger.error("Not closed the DataSource properly, cause: {}", e)
        throw new RuntimeException(e)
    } finally {
      RocksDbDataSource.dbLock.writeLock().unlock()
    }
  }

  /**
    * This function closes the DataSource, if it is not yet closed, and deletes all the files used by it.
    */
  override def destroy(): Unit = {
    try {
      close()
    } finally {
      import rocksDbConfig._

      val tableCfg = new BlockBasedTableConfig()
        .setBlockSize(blockSize)
        .setBlockCacheSize(blockCacheSize)
        .setCacheIndexAndFilterBlocks(true)
        .setPinL0FilterAndIndexBlocksInCache(true)
        .setFilter(new BloomFilter(10, false))

      val options = new Options()
        .setCreateIfMissing(createIfMissing)
        .setParanoidChecks(paranoidChecks)
        .setCompressionType(CompressionType.LZ4_COMPRESSION)
        .setBottommostCompressionType(CompressionType.ZSTD_COMPRESSION)
        .setLevelCompactionDynamicLevelBytes(levelCompaction)
        .setMaxOpenFiles(maxOpenFiles)
        .setIncreaseParallelism(maxThreads)
        .setTableFormatConfig(tableCfg)

      logger.debug(s"About to destroy DataSource in path: $path")
      RocksDB.destroyDB(path, options)
      options.close()
    }
  }
}

trait RocksDbConfig {
  val createIfMissing: Boolean
  val paranoidChecks: Boolean
  val path: String
  val maxThreads: Int
  val maxOpenFiles: Int
  val verifyChecksums: Boolean
  val levelCompaction: Boolean
  val blockSize: Long
  val blockCacheSize: Long
}

object RocksDbDataSource {
  /**
    * The rocksdb implementation acquires a lock from the operating system to prevent misuse
    */
  private val dbLock = new ReentrantReadWriteLock()

  private def createDB(rocksDbConfig: RocksDbConfig, namespaces: Seq[Namespace]):
  (RocksDB, mutable.Buffer[ColumnFamilyHandle], ReadOptions, DBOptions, ColumnFamilyOptions) = {
    import rocksDbConfig._
    import scala.collection.JavaConverters._

    RocksDB.loadLibrary()

    RocksDbDataSource.dbLock.writeLock().lock()
    try {
      val readOptions = new ReadOptions().setVerifyChecksums(rocksDbConfig.verifyChecksums)

      val tableCfg = new BlockBasedTableConfig()
        .setBlockSize(blockSize)
        .setBlockCacheSize(blockCacheSize)
        .setCacheIndexAndFilterBlocks(true)
        .setPinL0FilterAndIndexBlocksInCache(true)
        .setFilter(new BloomFilter(10, false))

      val options = new DBOptions()
        .setCreateIfMissing(createIfMissing)
        .setParanoidChecks(paranoidChecks)
        .setMaxOpenFiles(maxOpenFiles)
        .setIncreaseParallelism(maxThreads)
        .setCreateMissingColumnFamilies(true)

      val cfOpts =
        new ColumnFamilyOptions()
          .setCompressionType(CompressionType.LZ4_COMPRESSION)
          .setBottommostCompressionType(CompressionType.ZSTD_COMPRESSION)
          .setLevelCompactionDynamicLevelBytes(levelCompaction)
          .setTableFormatConfig(tableCfg)

      val cfDescriptors = List(new ColumnFamilyDescriptor(RocksDB.DEFAULT_COLUMN_FAMILY, cfOpts)) ++ namespaces.map { namespace =>
        new ColumnFamilyDescriptor(namespace.toArray, cfOpts)
      }

      val columnFamilyHandleList = mutable.Buffer.empty[ColumnFamilyHandle]

      (
        RocksDB.open(options, path, cfDescriptors.asJava, columnFamilyHandleList.asJava),
        columnFamilyHandleList,
        readOptions,
        options,
        cfOpts
      )
    } finally {
      RocksDbDataSource.dbLock.writeLock().unlock()
    }
  }

  def apply(rocksDbConfig: RocksDbConfig, namespaces: Seq[Namespace]): RocksDbDataSource = {
    val allNameSpaces = Seq(RocksDB.DEFAULT_COLUMN_FAMILY.toIndexedSeq) ++ namespaces
    val (db, handles, readOptions, dbOptions, cfOptions) = createDB(rocksDbConfig, namespaces)
    assert(allNameSpaces.size == handles.size)
    val handlesMap = allNameSpaces.zip(handles.toList).toMap
    new RocksDbDataSource(db, rocksDbConfig, readOptions, dbOptions, cfOptions, allNameSpaces, handlesMap)
  }
}
