package io.iohk.ethereum.blockchain.visitor

import io.iohk.ethereum.domain.Blockchain

import scala.annotation.tailrec

/**
 * A [[BlockchainVisitor]] that visits the whole blockchain sequentially, starting from block 0.
 * @param listener
 * @tparam R The result type
 */
class FullBlockchainVisitor[R](val listener: BlockchainListener[R]) extends BlockchainVisitor[R] {
  def run[B <: Blockchain](chain: B): R = {
    @tailrec
    def run(number: BigInt, bestBlockNumber: BigInt): Unit = {
      if(number <= bestBlockNumber) {
        for(block ← chain.getBlockByNumber(number)) {
          visitBlock(block)
        }

        run(number + 1, bestBlockNumber)
      }
    }

    run(0: BigInt, chain.getBestBlockNumber())

    listener.result
  }
}
