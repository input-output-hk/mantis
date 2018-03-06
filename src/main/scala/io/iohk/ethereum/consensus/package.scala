package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.consensus.blocks.BlockGenerator
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.{Block, BlockHeader}

import scala.reflect.ClassTag

/**
 * Provides everything related to consensus.
 * Different consensus protocols are implemented in sub-packages.
 */
package object consensus {
  final type GetBlockHeaderByHash = ByteString ⇒ Option[BlockHeader]
  final type GetNBlocksBack = (ByteString, Int) ⇒ Seq[Block]

  def wrongConsensusArgument[T <: Consensus : ClassTag](consensus: Consensus): Nothing = {
    val requiredClass = implicitly[ClassTag[T]].runtimeClass
    val msg = s"Consensus is of ${consensus.getClass} it should be of $requiredClass"
    throw new IllegalArgumentException(msg)
  }

  def wrongValidatorsArgument[T <: Validators : ClassTag](validators: Validators): Nothing = {
    val requiredClass = implicitly[ClassTag[T]].runtimeClass
    val msg = s"validators are of ${validators.getClass} it should be of $requiredClass"
    throw new IllegalArgumentException(msg)
  }

  def wrongBlockGeneratorArgument[T <: BlockGenerator : ClassTag](blockGenerator: BlockGenerator): Nothing = {
    val requiredClass = implicitly[ClassTag[T]].runtimeClass
    val msg = s"Block generator is of ${blockGenerator.getClass} it should be of $requiredClass"
    throw new IllegalArgumentException(msg)
  }

  // FIXME Put extra utilities here instead of polluting the primary interface
  final implicit class RichConsensus(val consensus: Consensus) extends AnyVal {
  }
}
