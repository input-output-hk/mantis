package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.testkit.TestKit
import akka.testkit.TestProbe
import akka.util.ByteString

import monix.eval.Task

import scala.collection.immutable.NumericRange

import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.FreeSpecBase
import io.iohk.ethereum.SpecFixtures
import io.iohk.ethereum.WithActorSystemShutDown
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.LegacyTransaction
import io.iohk.ethereum.domain.SignedTransactionWithSender
import io.iohk.ethereum.jsonrpc.MantisService.GetAccountTransactionsRequest
import io.iohk.ethereum.jsonrpc.MantisService.GetAccountTransactionsResponse
import io.iohk.ethereum.nodebuilder.ApisBuilder
import io.iohk.ethereum.nodebuilder.JSONRpcConfigBuilder
import io.iohk.ethereum.nodebuilder.MantisServiceBuilder
import io.iohk.ethereum.nodebuilder.PendingTransactionsManagerBuilder
import io.iohk.ethereum.nodebuilder.TransactionHistoryServiceBuilder
import io.iohk.ethereum.nodebuilder.TxPoolConfigBuilder
import io.iohk.ethereum.transactions.TransactionHistoryService
import io.iohk.ethereum.transactions.TransactionHistoryService.ExtendedTransactionData
import io.iohk.ethereum.transactions.TransactionHistoryService.MinedTransactionData
import io.iohk.ethereum.utils.BlockchainConfig

class MantisServiceSpec
    extends TestKit(ActorSystem("MantisServiceSpec"))
    with FreeSpecBase
    with SpecFixtures
    with WithActorSystemShutDown {
  class Fixture
      extends TransactionHistoryServiceBuilder.Default
      with EphemBlockchainTestSetup
      with PendingTransactionsManagerBuilder
      with TxPoolConfigBuilder
      with MantisServiceBuilder
      with JSONRpcConfigBuilder
      with ApisBuilder {
    lazy val pendingTransactionsManagerProbe: TestProbe = TestProbe()
    override lazy val pendingTransactionsManager: ActorRef = pendingTransactionsManagerProbe.ref
  }
  def createFixture() = new Fixture

  "Mantis Service" - {
    "should get account's transaction history" in {
      class TxHistoryFixture extends Fixture {
        val fakeTransaction = SignedTransactionWithSender(
          LegacyTransaction(
            nonce = 0,
            gasPrice = 123,
            gasLimit = 123,
            receivingAddress = Address("0x1234"),
            value = 0,
            payload = ByteString()
          ),
          signature = ECDSASignature(0, 0, 0.toByte),
          sender = Address("0x1234")
        )

        val block =
          BlockHelpers.generateBlock(BlockHelpers.genesis).copy(body = BlockBody(List(fakeTransaction.tx), Nil))

        val expectedResponse = List(
          ExtendedTransactionData(
            fakeTransaction.tx,
            isOutgoing = true,
            Some(MinedTransactionData(block.header, 0, 42, isCheckpointed = false))
          )
        )

        override lazy val transactionHistoryService: TransactionHistoryService =
          new TransactionHistoryService(
            blockchainReader,
            pendingTransactionsManager,
            txPoolConfig.getTransactionFromPoolTimeout
          ) {
            override def getAccountTransactions(account: Address, fromBlocks: NumericRange[BigInt])(implicit
                blockchainConfig: BlockchainConfig
            ) =
              Task.pure(expectedResponse)
          }
      }

      customTestCaseM(new TxHistoryFixture) { fixture =>
        import fixture._

        mantisService
          .getAccountTransactions(GetAccountTransactionsRequest(fakeTransaction.senderAddress, BigInt(0) to BigInt(1)))
          .map(result => assert(result === Right(GetAccountTransactionsResponse(expectedResponse))))
      }
    }

    "should validate range size against configuration" in testCaseM { fixture: Fixture =>
      import fixture._

      mantisService
        .getAccountTransactions(
          GetAccountTransactionsRequest(Address(1), BigInt(0) to BigInt(jsonRpcConfig.accountTransactionsMaxBlocks + 1))
        )
        .map(result => assert(result.isLeft))
    }
  }
}
