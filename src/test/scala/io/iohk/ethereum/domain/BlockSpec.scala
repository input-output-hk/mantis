package io.iohk.ethereum.domain

import io.iohk.ethereum.Fixtures.Blocks._
import org.scalatest.{FlatSpec, Matchers}

class BlockSpec extends FlatSpec with Matchers {
  "Block size" should "be correct" in {
    assert(Block.size(Genesis.block) == Genesis.size)
    assert(Block.size(Block3125369.block) == Block3125369.size)
    assert(Block.size(DaoForkBlock.block) == DaoForkBlock.size)
  }
}
