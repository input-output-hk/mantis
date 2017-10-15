package io.iohk.ethereum.db.dataSource

import java.io.File

import io.iohk.ethereum.common.{BatchOperation, Removal, Upsert}
import io.iohk.ethereum.db.dataSource.DataSource.{Key, Namespace, Value}
import org.iq80.leveldb.{DB, Options, WriteOptions}
import org.iq80.leveldb.impl.Iq80DBFactory


class LevelDBDataSource(
                         private var db: DB,
                         private val levelDbConfig: LevelDbConfig
                       )
  extends DataSource {

  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key
    * @return the value associated with the passed key.
    */
  override def get(namespace: Namespace, key: Key): Option[Value] = Option(db.get((namespace ++ key).toArray))

  /**
    * This function updates the DataSource by deleting, updating and inserting new (key-value) pairs.
    *
    * @param namespace from which the (key-value) pairs will be removed and inserted.
    * @param batchOperations sequence of operations to be applied
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(namespace: Namespace, batchOperations: Seq[BatchOperation[Key, Value]]): DataSource = {
    val batch = db.createWriteBatch()
    batchOperations.foreach {
      case Removal(key) => batch.delete((namespace ++ key).toArray)
      case Upsert(key, value) => batch.put((namespace ++ key).toArray, value.toArray)
    }

    db.write(batch, new WriteOptions())

    this
  }

  /**
    * This function updates the DataSource by deleting all the (key-value) pairs in it.
    *
    * @return the new DataSource after all the data was removed.
    */
  override def clear: DataSource = {
    destroy()
    this.db = LevelDBDataSource.createDB(levelDbConfig)
    this
  }

  /**
    * This function closes the DataSource, without deleting the files used by it.
    */
  override def close(): Unit = db.close()

  /**
    * This function closes the DataSource, if it is not yet closed, and deletes all the files used by it.
    */
  override def destroy(): Unit = {
    try {
      close()
    } finally {
      Iq80DBFactory.factory.destroy(new File(levelDbConfig.path), null) // Options are not being used ¯\_(ツ)_/¯
    }
  }
}

trait LevelDbConfig {
  val createIfMissing: Boolean
  val paranoidChecks: Boolean
  val verifyChecksums: Boolean
  val path: String
}

object LevelDBDataSource {

  private def createDB(levelDbConfig: LevelDbConfig): DB = {
    import levelDbConfig._

    val options = new Options()
      .createIfMissing(createIfMissing)
      .paranoidChecks(paranoidChecks) // raise an error as soon as it detects an internal corruption
      .verifyChecksums(verifyChecksums) // force checksum verification of all data that is read from the file system on behalf of a particular read

    Iq80DBFactory.factory.open(new File(path), options)
  }

  def apply(levelDbConfig: LevelDbConfig): LevelDBDataSource = {
    new LevelDBDataSource(createDB(levelDbConfig), levelDbConfig)
  }
}
