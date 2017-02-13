package io.iohk.ethereum.db.dataSource

import org.iq80.leveldb.Options
import org.scalatest.FlatSpec

class LevelDBDataSourceTest extends FlatSpec with DataSourceTestBehavior {

  private def createDataSource(path: String) = LevelDBDataSource(path, new LevelDbOptions {
    // Configs available https://rawgit.com/google/leveldb/master/doc/index.html
    override def buildOptions(): Options = new Options()
      .createIfMissing(true)
      .paranoidChecks(true)
      .verifyChecksums(true)
      .cacheSize(0)
  })

  it should behave like dataSource(createDataSource)
}
