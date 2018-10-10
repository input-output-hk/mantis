package io.iohk.ethereum.ledger

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{ Block, SignedTransaction }
import io.iohk.ethereum.ledger.Ledger.BlockResult
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.scalatest.{ Matchers, WordSpec }

class BlockExecutionSpec extends WordSpec with Matchers {

  "BlockExecution" should { // todo: move tests connected with block execution here (AK)

    "execute all block transaction correctly" in new BlockchainSetup {
      val blockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(validStxSignedByOrigin.tx))
      val block = Block(validBlockHeader, blockBodyWithTxs)

      val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
      val blockExecution = new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)

      val txsExecResult: Either[BlockExecutionError, BlockResult] = blockExecution.executeBlockTransactions(block)

      txsExecResult.isRight shouldBe true
    }

    "handle execution of block transaction when not all were executed correctly" in new BlockchainSetup {
      val invalidStx = SignedTransaction(validTx, ECDSASignature(1, 2, 3.toByte))
      val blockBodyWithTxs: BlockBody = validBlockBodyWithNoTxs.copy(transactionList = Seq(invalidStx))
      val block = Block(validBlockHeader, blockBodyWithTxs)

      val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
      val blockExecution = new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)

      val txsExecResult: Either[BlockExecutionError, BlockResult] = blockExecution.executeBlockTransactions(block)

      txsExecResult.isLeft shouldBe true
    }
  }
}
