package io.iohk.ethereum.db.dataSource

import java.util.concurrent.locks.ReentrantReadWriteLock
import io.iohk.ethereum.db.dataSource.DataSource._
import io.iohk.ethereum.utils.TryWithResources.withResources
import org.rocksdb._
import org.slf4j.LoggerFactory
import io.iohk.ethereum.db.dataSource.RocksDbDataSource._
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

  @volatile
  private var isClosed = false

  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key       the key retrieve the value.
    * @return the value associated with the passed key.
    */
  override def get(namespace: Namespace, key: Key): Option[Value] = {
    dbLock.readLock().lock()
    try {
      assureNotClosed()
      Option(db.get(handles(namespace), readOptions, key.toArray))
    } catch {
      case ex: RocksDbDataSourceClosedException =>
        throw ex
      case NonFatal(e) =>
        logger.error(s"Not found associated value to a namespace: $namespace and a key: $key, cause: {}", e.getMessage)
        throw e
    } finally {
      dbLock.readLock().unlock()
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
    dbLock.readLock().lock()
    try {
      assureNotClosed()
      Option(db.get(readOptions, key))
    } catch {
      case ex: RocksDbDataSourceClosedException =>
        throw ex
      case NonFatal(e) =>
        logger.error(s"Not found associated value to a key: $key, cause: {}", e.getMessage)
        throw e
    } finally {
      dbLock.readLock().unlock()
    }
  }

  override def update(dataSourceUpdates: Seq[DataUpdate]): Unit = {
    dbLock.readLock().lock()
    try {
      assureNotClosed()
      withResources(new WriteOptions()) { writeOptions =>
        withResources(new WriteBatch()) { batch =>
          dataSourceUpdates.foreach {
            case DataSourceUpdate(namespace, toRemove, toUpsert) =>
              toRemove.foreach { key =>
                batch.delete(handles(namespace), key.toArray)
              }
              toUpsert.foreach { case (k, v) => batch.put(handles(namespace), k.toArray, v.toArray) }

            case DataSourceUpdateOptimized(toRemove, toUpsert) =>
              toRemove.foreach { key =>
                batch.delete(key)
              }
              toUpsert.foreach { case (k, v) => batch.put(k, v) }
          }
          db.write(writeOptions, batch)
        }
      }
    } catch {
      case ex: RocksDbDataSourceClosedException =>
        throw ex
      case NonFatal(e) =>
        logger.error(
          s"DataSource not updated, cause: {}",
          e.getMessage
        )
        throw e
    } finally {
      dbLock.readLock().unlock()
    }
  }

  /**
    * This function updates the DataSource by deleting all the (key-value) pairs in it.
    */
  override def clear(): Unit = {
    destroy()
    logger.debug(s"About to create new DataSource for path: ${rocksDbConfig.path}")
    val (newDb, handles, readOptions, dbOptions, cfOptions) = createDB(rocksDbConfig, nameSpaces.tail)

    assert(nameSpaces.size == handles.size)

    this.db = newDb
    this.readOptions = readOptions
    this.handles = nameSpaces.zip(handles.toList).toMap
    this.dbOptions = dbOptions
    this.cfOptions = cfOptions
    this.isClosed = false
  }

  /**
    * This function closes the DataSource, without deleting the files used by it.
    */
  override def close(): Unit = {
    logger.info(s"About to close DataSource in path: ${rocksDbConfig.path}")
    assureNotClosed()
    dbLock.writeLock().lock()
    isClosed = true
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
        throw e
    } finally {
      dbLock.writeLock().unlock()
    }
  }

  /**
    * This function closes the DataSource, if it is not yet closed, and deletes all the files used by it.
    */
  override def destroy(): Unit = {
    try {
      if (!isClosed) {
        close()
      }
    } finally {
      destroyDB()
    }
  }

  protected def destroyDB(): Unit = {
    try {
      import rocksDbConfig._
      val tableCfg = new BlockBasedTableConfig()
        .setBlockSize(blockSize)
        .setBlockCache(new ClockCache(blockCacheSize))
        .setCacheIndexAndFilterBlocks(true)
        .setPinL0FilterAndIndexBlocksInCache(true)
        .setFilterPolicy(new BloomFilter(10, false))

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
    } catch {
      case NonFatal(e) =>
        logger.error("Not destroyed the DataSource properly, cause: {}", e)
        throw e
    }
  }

  private def assureNotClosed(): Unit = {
    if (isClosed) {
      logger.warn("database is closed")
      throw new RocksDbDataSourceClosedException(s"This ${getClass.getSimpleName} has been closed")
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

  class RocksDbDataSourceClosedException(val message: String) extends IllegalStateException(message)

  private val logger = LoggerFactory.getLogger("rocks-db")

  /**
    * The rocksdb implementation acquires a lock from the operating system to prevent misuse
    */
  private val dbLock = new ReentrantReadWriteLock()

  // scalastyle:off method.length
  private def createDB(
      rocksDbConfig: RocksDbConfig,
      namespaces: Seq[Namespace]
  ): (RocksDB, mutable.Buffer[ColumnFamilyHandle], ReadOptions, DBOptions, ColumnFamilyOptions) = {
    import rocksDbConfig._
    import scala.collection.JavaConverters._

    RocksDB.loadLibrary()

    RocksDbDataSource.dbLock.writeLock().lock()
    try {
      val readOptions = new ReadOptions().setVerifyChecksums(rocksDbConfig.verifyChecksums)

      val tableCfg = new BlockBasedTableConfig()
        .setBlockSize(blockSize)
        .setBlockCache(new ClockCache(blockCacheSize))
        .setCacheIndexAndFilterBlocks(true)
        .setPinL0FilterAndIndexBlocksInCache(true)
        .setFilterPolicy(new BloomFilter(10, false))

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

      val cfDescriptors = List(new ColumnFamilyDescriptor(RocksDB.DEFAULT_COLUMN_FAMILY, cfOpts)) ++ namespaces.map {
        namespace =>
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
    } catch {
      case NonFatal(e) =>
        logger.error("Not created the DataSource properly, cause: {}", e)
        throw e
    } finally {
      RocksDbDataSource.dbLock.writeLock().unlock()
    }
  }

  def apply(rocksDbConfig: RocksDbConfig, namespaces: Seq[Namespace]): RocksDbDataSource = {
    val allNameSpaces = Seq(RocksDB.DEFAULT_COLUMN_FAMILY.toIndexedSeq) ++ namespaces
    val (db, handles, readOptions, dbOptions, cfOptions) = createDB(rocksDbConfig, namespaces)
    assert(allNameSpaces.size == handles.size)
    val handlesMap = allNameSpaces.zip(handles.toList).toMap
    //This assert ensures that we do not have duplicated namespaces
    assert(handlesMap.size == handles.size)
    new RocksDbDataSource(db, rocksDbConfig, readOptions, dbOptions, cfOptions, allNameSpaces, handlesMap)
  }
}
