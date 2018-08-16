package io.iohk.ethereum.blockchain.visitor

import io.iohk.ethereum.domain.{Address, Block, Blockchain, SignedTransaction}

/**
 * A visitor for the blockchain structure that uses a
 * [[BlockchainListener]] to compute a result.
 *
 * @tparam R The result type
 */
trait BlockchainVisitor[R] {
  def listener: BlockchainListener[R]

  def run[B <: Blockchain](chain: B): R

  def visitBlock(block: Block): Unit = {
    listener.enterBlock(block)

    visitBlockHeader(block)
    visitBlockBody(block)

    listener.leaveBlock(block)
  }

  def visitBlockHeader(block: Block): Unit = {
    listener.enterBlockHeader(block)
    listener.leaveBlockHeader(block)
  }

  def visitBlockBody(block: Block): Unit = {
    listener.enterBlockBody(block)

    visitTransactions(block)
    // visitUncles() ... but not needed now ...

    listener.leaveBlockBody(block)
  }

  def visitTransactions(block: Block): Unit = {
    val body = block.body
    val stxs = body.transactionList
    val stxn = stxs.length

    listener.enterTransactions(block)

    for((stx, stxi) ← stxs.zipWithIndex) {
      visitTransaction(block, stx, stxi, stxn)
    }

    listener.leaveTransactions(block)
  }

  def visitTransaction(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int): Unit = {
    listener.enterTransaction(block, stx, stxi, stxn)

    stx.tx.receivingAddress match {
      case None ⇒
        assert(stx.tx.isContractInit)
        visitContractTx(block, stx, stxi, stxn)

      case Some(address) ⇒
        visitNonContractTx(block, stx, stxi, stxn, address)
    }

    listener.leaveTransaction(block, stx, stxi, stxn)
  }

  def visitContractTx(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int): Unit = {
    listener.enterContractTx(block, stx, stxi, stxn)
    listener.leaveContractTx(block, stx, stxi, stxn)
  }

  def visitNonContractTx(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int, receivingAddress: Address): Unit = {
    listener.enterNonContractTx(block, stx, stxi, stxn, receivingAddress)
    listener.leaveNonContractTx(block, stx, stxi, stxn, receivingAddress)
  }
}
