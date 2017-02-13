package io.iohk.ethereum.db.dataSource
import io.iohk.ethereum.utils.Config.Db.LevelDb.{cacheSize, createIfMissing, paranoidChecks, verifyChecksums}
import org.iq80.leveldb.Options

object ConfigFileLevelDbOptions extends LevelDbOptions {
  // Configs available https://rawgit.com/google/leveldb/master/doc/index.html
  override def buildOptions(): Options = new Options()
    .createIfMissing(createIfMissing)
    .paranoidChecks(paranoidChecks)
    .verifyChecksums(verifyChecksums)
    .cacheSize(cacheSize)
}
