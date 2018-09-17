package io.iohk.ethereum.db.dataSource

import java.io.File

import org.iq80.leveldb.impl.Iq80DBFactory
import org.iq80.leveldb.{ DB, Options }
import io.iohk.ethereum.db.dataSource.DataSource._
class LevelDBDataSource(private var db: DB, private val levelDbConfig: LevelDbConfig) extends DataSource {

  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key       the key retrieve the value.
    * @return the value associated with the passed key.
    */
  override def get(namespace: Namespace, key: Key): Option[Value] = Option(db.get((namespace ++ key).toArray))

  /**
    * This function obtains the associated value to a key, if there exists one. It assumes that
    * caller already properly serialized key. Useful when caller knows some pattern in data to
    * avoid generic serialization.
    *
    * @param key the key retrieve the value.
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
    val batch = db.createWriteBatch()

    toRemove.foreach{ key => batch.delete((namespace ++ key).toArray) }
    toUpsert.foreach{ case (k, v) => batch.put((namespace ++ k).toArray, v.toArray) }

    db.write(batch)
    batch.close()

    this
  }

  override def updateOptimized(toRemove: Seq[Array[Byte]], toUpsert: Seq[(Array[Byte], Array[Byte])]): DataSource = {
    val batch = db.createWriteBatch()

    toRemove.foreach{ key => batch.delete(key) }
    toUpsert.foreach{ case (k, v) => batch.put(k, v) }

    db.write(batch)
    batch.close()

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
      import levelDbConfig._

      Iq80DBFactory.factory.destroy(new File(path), null) // options are not used here
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
