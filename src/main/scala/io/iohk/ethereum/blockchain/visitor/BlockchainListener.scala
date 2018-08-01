package io.iohk.ethereum.blockchain.visitor

import io.iohk.ethereum.domain.{Address, Block, SignedTransaction}

/**
 * Listens to events that a [[io.iohk.ethereum.blockchain.visitor.BlockchainVisitor visitor]] generates,
 * gradually computing a result of type `R`.
 *
 * @tparam R The result type
 */
trait BlockchainListener[R] {
  def result: R

  def enterBlock(block: Block): Unit = {}
  def leaveBlock(block: Block): Unit = {}

  def enterBlockHeader(block: Block): Unit = {}
  def leaveBlockHeader(block: Block): Unit = {}

  def enterBlockBody(block: Block): Unit = {}
  def leaveBlockBody(block: Block): Unit = {}

  def enterTransactions(block: Block): Unit = {}
  def leaveTransactions(block: Block): Unit = {}

  def enterTransaction(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int): Unit = {}
  def leaveTransaction(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int): Unit = {}

  def enterContractTx(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int): Unit = {}
  def leaveContractTx(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int): Unit = {}

  def enterNonContractTx(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int, receivingAddress: Address): Unit = {}
  def leaveNonContractTx(block: Block, stx: SignedTransaction, stxi: Int, stxn: Int, receivingAddress: Address): Unit = {}
}
