package io.iohk.ethereum.db.dataSource

import java.io.File
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicLong

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

class IodbDataSource private (lSMStore: LSMStore,
                              keySize: Int,
                              firstVersion: ByteArrayWrapper,
                              dir: File) extends DataSource {

  import IodbDataSource._

  override def get(namespace: Namespace, key: Key): Option[Value] = {
    require(namespace.length + key.length == keySize, "Wrong key size in IODB get")
    lSMStore.get(ByteArrayWrapper((namespace ++ key).toArray)).map(v => v.data.toIndexedSeq)
  }

  override def update(namespace: Namespace, toRemove: Seq[Key], toUpsert: Seq[(Key, Value)]): DataSource = {
    require(
      toRemove.forall{ keyToRemove => namespace.length + keyToRemove.length == keySize } &&
        toUpsert.forall{ case (keyToUpSert, _) => namespace.length + keyToUpSert.length == keySize },
      "Wrong key size in IODB update"
    )
    lSMStore.update(
      ByteArrayWrapper(storageVersionGen()),
      toRemove.map(key => ByteArrayWrapper((namespace ++ key).toArray)),
      asStorables(namespace, toUpsert))
    new IodbDataSource(lSMStore, keySize, firstVersion, dir)
  }

  override def clear: DataSource = {
    lSMStore.rollback(firstVersion)
    new IodbDataSource(lSMStore, keySize, firstVersion, dir)
  }

  override def close(): Unit = {
    lSMStore.close()
    deleteDirectory(dir)
  }

  private def asStorables(namespace: Namespace,
                          keyValues: Seq[(Key, Value)]): Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
    keyValues.map(kv => ByteArrayWrapper((namespace ++ kv._1).toArray) -> ByteArrayWrapper(kv._2.toArray))
}


object IodbDataSource {
  private val updateCounter = new AtomicLong(System.currentTimeMillis())

  private def storageVersionGen(): Array[Byte] = {
    ByteBuffer.allocate(java.lang.Long.SIZE / java.lang.Byte.SIZE).putLong(updateCounter.incrementAndGet()).array()
  }

  def apply(path: String, keySize: Int): IodbDataSource = {
    //Delete directory if it existed and create a new one
    val dir: File = new File(path)
    val dirCreationSuccess = (!dir.exists() || deleteDirectory(dir)) && dir.mkdir()
    assert(dirCreationSuccess, "Iodb folder creation failed")

    val lSMStore: LSMStore = new LSMStore(dir = dir, keySize = keySize, keepSingleVersion = true)

    //Obtain first LSMStore version
    lSMStore.update(ByteArrayWrapper(storageVersionGen()), Seq(), Seq())
    val firstVersion: ByteArrayWrapper = lSMStore.lastVersionID
      .getOrElse(throw new Exception("Iodb obtaining of first version failed"))

    new IodbDataSource(lSMStore, keySize, firstVersion, dir)
  }

  private def deleteDirectory(dir: File): Boolean = {
    require(dir.isDirectory, "Called on a file that is not a directory")
    val files = Option(dir.listFiles()).getOrElse(Array())
    val filesDeletionSuccess: Boolean = files.map { f =>
      if(f.isDirectory) deleteDirectory(f) else f.delete()
    }.forall(identity)
    filesDeletionSuccess && dir.delete()
  }
}
