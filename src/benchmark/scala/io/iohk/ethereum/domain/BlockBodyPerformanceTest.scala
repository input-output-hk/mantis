package io.iohk.ethereum.domain

import io.iohk.ethereum.blockchain.sync.StateSyncUtils
import io.iohk.ethereum.utils.GenOps.GenOps
import io.iohk.ethereum.{BlockHelpers, ObjectGenerators}
import io.iohk.ethereum.utils.Logger
import org.scalatest.wordspec.AnyWordSpec
import util.DeepSize

class BlockBodyPerformanceTest extends AnyWordSpec with Logger {

  private lazy val (stateRoot, trieProvider) = {
    val stateNodesData = ObjectGenerators.genMultipleNodeData(20).pickValue

    lazy val trieProvider = StateSyncUtils.TrieProvider()
    lazy val stateRoot = trieProvider.buildWorld(stateNodesData)

    (stateRoot, trieProvider)
  }

  private lazy val testBlocks = BlockHelpers.generateChain(
    10000,
    BlockHelpers.genesis,
    block => block.copy(header = block.header.copy(stateRoot = stateRoot))
  )

//  val blocks = (0 until 1000000).map { _ =>
//    BlockHelpers.generateBlock()
//
//  }


  "It" should {
    "Calculate deep size" in {
      log.warn("Some size is: " + DeepSize.apply(testBlocks))
    }
  }


}
