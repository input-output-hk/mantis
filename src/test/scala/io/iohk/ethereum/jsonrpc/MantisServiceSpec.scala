package io.iohk.ethereum.jsonrpc

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Address, BlockBody, SignedTransactionWithSender, Transaction}
import io.iohk.ethereum.jsonrpc.MantisService.{GetAccountTransactionsRequest, GetAccountTransactionsResponse}
import io.iohk.ethereum.nodebuilder.{
  ApisBuilder,
  JSONRpcConfigBuilder,
  MantisServiceBuilder,
  PendingTransactionsManagerBuilder,
  TransactionHistoryServiceBuilder,
  TxPoolConfigBuilder
}
import io.iohk.ethereum.transactions.TransactionHistoryService
import io.iohk.ethereum.transactions.TransactionHistoryService.{ExtendedTransactionData, MinedTransactionData}
import io.iohk.ethereum.{BlockHelpers, FreeSpecBase, SpecFixtures, WithActorSystemShutDown}
import monix.eval.Task

import scala.collection.immutable.NumericRange

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
    lazy val pendingTransactionsManagerProbe = TestProbe()
    override lazy val pendingTransactionsManager: ActorRef = pendingTransactionsManagerProbe.ref
  }
  def createFixture() = new Fixture

  "Mantis Service" - {
    "should get account's transaction history" in {
      class TxHistoryFixture extends Fixture {
        val fakeTransaction = SignedTransactionWithSender(
          Transaction(
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
            blockchain,
            pendingTransactionsManager,
            txPoolConfig.getTransactionFromPoolTimeout
          ) {
            override def getAccountTransactions(account: Address, fromBlocks: NumericRange[BigInt]) =
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
