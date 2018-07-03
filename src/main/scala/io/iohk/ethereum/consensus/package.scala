package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.consensus.blocks.BlockGenerator
import io.iohk.ethereum.consensus.ethash.EthashConsensus
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

  final implicit class RichConsensus(val consensus: Consensus) extends AnyVal {
    /**
     * There are APIs that expect that the standard Ethash consensus is running and so depend
     * on either its configuration or general PoW semantics.
     * This is a method that can handle such cases via a respective if/then/else construct:
     * if we run under [[io.iohk.ethereum.consensus.ethash.EthashConsensus EthashConsensus]]
     * then the `_then` function is called, otherwise the `_else` value is computed.
     */
    def ifEthash[A](_then: EthashConsensus ⇒ A)(_else: ⇒ A): A =
      consensus match {
        case ethash: EthashConsensus ⇒ _then(ethash)
        case _ ⇒ _else
      }
  }
}
