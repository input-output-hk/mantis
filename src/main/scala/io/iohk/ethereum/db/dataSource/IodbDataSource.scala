package io.iohk.ethereum.db.dataSource

import java.io.File
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicLong

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

class IodbDataSource private (lSMStore: LSMStore, keySize: Int, path: String) extends DataSource {

  import IodbDataSource._

  override def get(namespace: Namespace, key: Key): Option[Value] = {
    require(namespace.length + key.length <= keySize, "Wrong key size in IODB get")
    val keyPadded = padToKeySize(namespace, key, keySize).toArray
    lSMStore.get(ByteArrayWrapper(keyPadded)).map(v => v.data.toIndexedSeq)
  }

  override def update(namespace: Namespace, toRemove: Seq[Key], toUpsert: Seq[(Key, Value)]): DataSource = {
    require(
      toRemove.forall{ keyToRemove => namespace.length + keyToRemove.length <= keySize } &&
        toUpsert.forall{ case (keyToUpSert, _) => namespace.length + keyToUpSert.length <= keySize },
      "Wrong key size in IODB update"
    )
    val toRemovePadded = toRemove.map(key => padToKeySize(namespace, key, keySize))
    val toUpsertPadded = toUpsert.map{case (key, value) => padToKeySize(namespace, key, keySize) -> value}
    lSMStore.update(
      ByteArrayWrapper(storageVersionGen()),
      toRemovePadded.map(key => ByteArrayWrapper(key.toArray)),
      asStorables(toUpsertPadded))
    new IodbDataSource(lSMStore, keySize, path)
  }

  override def clear: DataSource = {
    destroy()
    IodbDataSource(path, keySize)
  }

  override def close(): Unit = lSMStore.close()

  override def destroy(): Unit = {
    try {
      close()
    } finally {
      val directoryDeletionSuccess = deleteDirectory(new File(path))
      assert(directoryDeletionSuccess, "Iodb folder destruction failed")
    }
  }

  private def asStorables(keyValues: Seq[(Key, Value)]): Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
    keyValues.map(kv => ByteArrayWrapper(kv._1.toArray) -> ByteArrayWrapper(kv._2.toArray))
}


object IodbDataSource {
  val KeySizeWithoutNamespace = 32
  val KeySize = 33

  private val updateCounter = new AtomicLong(System.currentTimeMillis())

  private def storageVersionGen(): Array[Byte] = {
    ByteBuffer.allocate(java.lang.Long.SIZE / java.lang.Byte.SIZE).putLong(updateCounter.incrementAndGet()).array()
  }

  /**
    * This function constructs an IodbDataSource.
    *
    * @param path of the folder where the DataSource files will be stored.
    * @param keySize of the keys to be stored in the DataSource.
    *                This length includes the length of the namespace and the length of the keys inside this namespace
    * @return an IodbDataSource.
    */
  def apply(path: String, keySize: Int): IodbDataSource = {
    val dir: File = new File(path)
    val dirSetupSuccess = (dir.exists() && dir.isDirectory) || dir.mkdirs()
    assert(dirSetupSuccess, "Iodb folder creation failed")

    val lSMStore: LSMStore = new LSMStore(dir = dir, keySize = keySize)
    new IodbDataSource(lSMStore, keySize, path)
  }

  private def deleteDirectory(dir: File): Boolean = {
    require(dir.exists() && dir.isDirectory, "Trying to delete a file thats not a folder")
    val files = dir.listFiles()
    val filesDeletionSuccess = files.forall { f =>
      val deleted = if (f.isDirectory) deleteDirectory(f) else f.delete()
      deleted
    }
    filesDeletionSuccess && dir.delete()
  }

  private def padToKeySize(namespace: IndexedSeq[Byte], key: IndexedSeq[Byte], keySize: Int): IndexedSeq[Byte] =
    namespace ++ key.padTo(keySize - namespace.size, 0.toByte)
}
