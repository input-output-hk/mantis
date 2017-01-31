package io.iohk.ethereum.db.dataSource

import java.io.File

import org.iq80.leveldb.{DB, Options, WriteOptions}
import org.iq80.leveldb.impl.{Iq80DBFactory, WriteBatchImpl}


class LevelDBDataSource(val db: DB, private val path: String) extends DataSource {

  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key
    * @return the value associated with the passed key.
    */
  override def get(namespace: Namespace, key: Key): Option[Value] = Option(db.get(key.toArray))

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
    val batch = new WriteBatchImpl()
    toRemove.foldLeft(batch) { (batch, key) => batch.delete(key.toArray[Byte]); batch }
    toUpsert.foldLeft(batch) { (batch, item) => db.put(item._1.toArray, item._2.toArray); batch }
    db.write(batch, new WriteOptions())
    new LevelDBDataSource(db, path)
  }

  /**
    * This function updates the DataSource by deleting all the (key-value) pairs in it.
    *
    * @return the new DataSource after all the data was removed.
    */
  override def clear: DataSource = {
    destroy()
    LevelDBDataSource(path)
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
      Iq80DBFactory.factory.destroy(new File(path), null) // Options are not being used ¯\_(ツ)_/¯
    }
  }
}


object LevelDBDataSource {

  def apply(path: String): LevelDBDataSource = {
    // Configs available https://rawgit.com/google/leveldb/master/doc/index.html
    val options = new Options()
      .createIfMissing(true)
      .paranoidChecks(true) // raise an error as soon as it detects an internal corruption
      .verifyChecksums(true) // force checksum verification of all data that is read from the file system on behalf of a particular read
      .cacheSize(0) // do not cache
    new LevelDBDataSource(Iq80DBFactory.factory.open(new File(path), options), path)
  }
}
