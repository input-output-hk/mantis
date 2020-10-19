package io.iohk.ethereum.consensus.ethash

import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPSerializable}

package object blocks {

  /**
    * This is type `X` in `BlockGenerator`.
    *
    * @see [[io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator EthashBlockGenerator]],
    *      [[io.iohk.ethereum.consensus.blocks.BlockGenerator.X BlockGenerator{ type X}]]
    */
  final type Ommers = Seq[BlockHeader]

  implicit class OmmersSeqEnc(blockHeaders: Seq[BlockHeader]) extends RLPSerializable {
    override def toRLPEncodable: RLPEncodeable = RLPList(blockHeaders.map(_.toRLPEncodable): _*)
  }
}
