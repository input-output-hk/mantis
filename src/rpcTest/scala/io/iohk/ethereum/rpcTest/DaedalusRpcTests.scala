package io.iohk.ethereum.rpcTest
import io.iohk.ethereum.rpcTest.data.TestData.{firstAccount, secondAccount, unexistingAccount}
import io.iohk.ethereum.rpcTest.utils.{MantisProcess, ProcessUtils, RpcTestConfigLoader}
import monix.eval.Task
import org.scalatest.{BeforeAndAfterEach, FreeSpec, Matchers}
import utils.ResourceOps._
import cats.syntax.traverse._
import cats.instances.list._
import monix.execution.Scheduler
import mouse.all._

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration
import scala.sys.process.Process
import scala.concurrent.duration._

class DaedalusRpcTests extends FreeSpec with Matchers with BeforeAndAfterEach with RpcTestConfigLoader {
  val defaultNetwork = "rpc-test-private"
  val defaultTimeout: FiniteDuration = 1.minute

  implicit val scheduler: Scheduler = monix.execution.Scheduler(scala.concurrent.ExecutionContext.global)
  var processes: mutable.Buffer[Process] = _

  override def beforeEach(): Unit =
    processes = mutable.ArrayBuffer()

  override def afterEach(): Unit = processes.toList.traverse(ProcessUtils.kill).runSyncUnsafe()

  "daedalus_" - {
    "changePassphrase" - {
      val oldPassphrase = "Zaq12wsxcde3"
      val newPassphrase = "b" * 7

      "errors on non-existing wallet" in new ScenarioSetup with RawHttpRpc {
        (MantisProcess.create(), makeHttp())
          .use2((proc, http) => {
            for {
              _ <- proc.waitForRpc()
              res <- http.call("daedalus_changePassphrase", unexistingAccount.address, "a" * 7, "b" * 7)
            } yield getErrorCode(res) shouldBe Right(-32000)
          })
          .runSyncUnsafe()
      }

      "doesn't change passphrase if old one is invalid" in new ScenarioSetup with RawHttpRpc {
        (MantisProcess.create(), makeHttp())
          .use2((proc, http) => {
            for {
              _ <- proc.waitForRpc()
              addr <- Task { service.personalNewAccount(oldPassphrase).send().getAccountId }
              changePassphraseRes <- http.call("daedalus_changePassphrase", addr, "a" * 7, newPassphrase)
              unlockNewRes <- Task { service.personalUnlockAccount(addr, newPassphrase).send() }
              unlockOldRes <- Task { service.personalUnlockAccount(addr, oldPassphrase).send() }
            } yield {
              getErrorCode(changePassphraseRes) shouldBe Right(-32000)
              unlockNewRes.hasError shouldBe true
              unlockOldRes.accountUnlocked() shouldBe true
            }
          })
          .runSyncUnsafe()
      }

      "changes passphrase" in new ScenarioSetup with RawHttpRpc {
        (MantisProcess.create(), makeHttp()).use2((proc, http) => {
          for {
            _ <- proc.waitForRpc()
            addr <- Task { service.personalNewAccount(oldPassphrase).send().getAccountId }
            _ <- http.call("daedalus_changePassphrase", addr, oldPassphrase, newPassphrase)
            unlockWrongPassRes <- Task { service.personalUnlockAccount(addr, "a" * 7).send() }
            unlockRightPassRes <- Task { service.personalUnlockAccount(addr, newPassphrase).send() }
          } yield {
            unlockWrongPassRes.hasError shouldBe true
            unlockRightPassRes.accountUnlocked() shouldBe true
          }
        }).runSyncUnsafe()
      }

      "changes passphrase of account defined in genesis" in new ScenarioSetup with RawHttpRpc {
        (MantisProcess.withTemporaryDatadir().create(), makeHttp()).use2((proc, http) => {
          for {
            _ <- proc.waitForRpc()
            _ <- http.call("daedalus_changePassphrase", firstAccount.address, firstAccount.password, newPassphrase)
            unlockWrongPassRes <- Task { service.personalUnlockAccount(firstAccount.address, "a" * 7).send() }
            unlockRightPassRes <- Task { service.personalUnlockAccount(firstAccount.address, newPassphrase).send() }
          } yield {
            unlockWrongPassRes.hasError shouldBe true
            unlockRightPassRes.accountUnlocked() shouldBe true
          }
        }).runSyncUnsafe()
      }

      "fails with missing new password" in new ScenarioSetup with RawHttpRpc {
        (MantisProcess.create(), makeHttp()).use2((proc, http) => {
          for {
            _ <- proc.waitForRpc()
            res <- http.call("daedalus_changePassphrase", firstAccount.address, "a" * 7)
          } yield getErrorCode(res) shouldBe Right(-32602)
        }).runSyncUnsafe()
      }

      "fails with missing passwords" in new ScenarioSetup with RawHttpRpc {
        (MantisProcess.create(), makeHttp()).use2((proc, http) => {
          for {
            _ <- proc.waitForRpc()
            res <- http.call("daedalus_changePassphrase", firstAccount.address)
          } yield getErrorCode(res) shouldBe Right(-32602)
        }).runSyncUnsafe()
      }
    }

    "deleteWallet" - {
      "errors on non-existing wallet" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .runToFuture |> (Await.result(_, defaultTimeout))

        val cursor = call("daedalus_deleteWallet", unexistingAccount.address) |> (Await.result(_, defaultTimeout))

        cursor.downField("error").downField("code").as[Int] shouldBe Right(-32000)
      }

      "deletes wallet created by personal_newAccount" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .runToFuture |> (Await.result(_, defaultTimeout))
        val pass = "Zaq12wsxcde3"

        val newAcc = service.personalNewAccount(pass).send().getAccountId
        call("daedalus_deleteWallet", newAcc) |>
          (Await.result(_, defaultTimeout)) |>
          (_.downField("result").as[Boolean] shouldBe Right(true))
        service.personalUnlockAccount(newAcc, pass, 10000).send().hasError shouldBe true
      }

      "deletes genesis wallet" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .runToFuture |> (Await.result(_, defaultTimeout))

        call("daedalus_deleteWallet", firstAccount.address) |>
          (Await.result(_, defaultTimeout)) |>
          (_.downField("result").as[Boolean] shouldBe Right(true))
        service.personalUnlockAccount(firstAccount.address, firstAccount.password, 10000).send().hasError shouldBe true
      }
    }

    "getAccountTransactions" - {
      "lists not mined transactions" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningDisabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .runToFuture |> (Await.result(_, defaultTimeout))

        val tx1 = valueTransfer(firstAccount.address, secondAccount.address, BigInt(10))
        val tx2 = valueTransfer(secondAccount.address, firstAccount.address, BigInt(42))

        val hash1 = service.personalSendTransaction(tx1, firstAccount.password).send().getTransactionHash
        val hash2 = service.personalSendTransaction(tx2, secondAccount.password).send().getTransactionHash

        val expectedHashes = Set(hash1, hash2)
        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, 0, 2) |>
          (Await.result(_, defaultTimeout))

        val actualHashes: Set[String] = cursor
          .downField("result")
          .downField("transactions")
          .values
          .map(_.flatMap(transaction => {
            transaction.hcursor.downField("hash").as[String].toOption
          }))
          .getOrElse(Vector())
          .toSet

        actualHashes should contain allElementsOf expectedHashes
      }

      "lists mined transactions" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .flatMap(_.waitForDag())
          .runSyncUnsafe()

        val tx1 = valueTransfer(firstAccount.address, secondAccount.address, BigInt(10))
        val tx2 = valueTransfer(secondAccount.address, firstAccount.address, BigInt(42))

        val hash1 = service.personalSendTransaction(tx1, firstAccount.password).send() |> { res =>
          res.getTransactionHash
        }
        val hash2 = service.personalSendTransaction(tx2, secondAccount.password).send().getTransactionHash

        List(hash1, hash2).traverse(waitForTransaction).runSyncUnsafe()

        val expectedHashes = Set(hash1, hash2)
        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, 0, 2) |>
          (Await.result(_, defaultTimeout))

        val actualHashes: Set[String] = cursor
          .downField("result")
          .downField("transactions")
          .values
          .map(_.flatMap(transaction => {
            transaction.hcursor.downField("hash").as[String].toOption
          }))
          .getOrElse(Vector())
          .toSet

        actualHashes should contain allElementsOf expectedHashes
      }

      "lists mined transaction from single block" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .flatMap(_.waitForDag())
          .runSyncUnsafe()

        val tx1 = valueTransfer(firstAccount.address, secondAccount.address, BigInt(10))

        val hash = service.personalSendTransaction(tx1, firstAccount.password).send().getTransactionHash

        val blockNr = waitForTransaction(hash).runSyncUnsafe().getBlockNumber

        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, blockNr, blockNr) |>
          (Await.result(_, defaultTimeout))

        val actualHashes: Set[String] = cursor
          .downField("result")
          .downField("transactions")
          .values
          .map(_.flatMap(transaction => {
            transaction.hcursor.downField("hash").as[String].toOption
          }))
          .getOrElse(Vector())
          .toSet

        actualHashes shouldBe Set(hash)
      }

      "lists contract transactions" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .flatMap(_.waitForDag())
          .runSyncUnsafe()

        val tx1 = createContract(firstAccount.address, secondAccount.address)

        val hash = service.personalSendTransaction(tx1, firstAccount.password).send().getTransactionHash

        waitForTransaction(hash).runSyncUnsafe()

        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, 0, 2) |>
          (Await.result(_, defaultTimeout))

        val actualHashes: Set[String] = cursor
          .downField("result")
          .downField("transactions")
          .values
          .map(_.flatMap(transaction => {
            transaction.hcursor.downField("hash").as[String].toOption
          }))
          .getOrElse(Vector())
          .toSet

        actualHashes shouldBe Set(hash)
      }

      "lists no transactions from non-existing account" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningDisabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .runSyncUnsafe()

        val cursor = call("daedalus_getAccountTransactions", unexistingAccount.address, 0, 100) |>
          (Await.result(_, defaultTimeout))

        val actualHashes: Set[String] = cursor
          .downField("result")
          .downField("transactions")
          .values
          .map(_.flatMap(transaction => {
            transaction.hcursor.downField("hash").as[String].toOption
          }))
          .getOrElse(Vector())
          .toSet

        actualHashes shouldBe 'empty
      }

      "allows for passing block number in hex" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .flatMap(_.waitForDag())
          .runSyncUnsafe()

        val tx1 = valueTransfer(firstAccount.address, secondAccount.address, BigInt(10))
        val tx2 = valueTransfer(secondAccount.address, firstAccount.address, BigInt(42))

        val hash1 = service.personalSendTransaction(tx1, firstAccount.password).send().getTransactionHash
        val hash2 = service.personalSendTransaction(tx2, secondAccount.password).send().getTransactionHash

        List(hash1, hash2).traverse(waitForTransaction).runSyncUnsafe()

        val expectedHashes = Set(hash1, hash2)
        val cursor1 = call("daedalus_getAccountTransactions", firstAccount.address, BigInt(0), 2) |>
          (Await.result(_, defaultTimeout))

        val actualHashes1: Set[String] = cursor1
          .downField("result")
          .downField("transactions")
          .values
          .map(_.flatMap(transaction => {
            transaction.hcursor.downField("hash").as[String].toOption
          }))
          .getOrElse(Vector())
          .toSet

        val cursor2 = call("daedalus_getAccountTransactions", firstAccount.address, 0, BigInt(2)) |>
          (Await.result(_, defaultTimeout))

        val actualHashes2: Set[String] = cursor1
          .downField("result")
          .downField("transactions")
          .values
          .map(_.flatMap(transaction => {
            transaction.hcursor.downField("hash").as[String].toOption
          }))
          .getOrElse(Vector())
          .toSet

        actualHashes1 should be(actualHashes2)
        actualHashes2 should contain allElementsOf expectedHashes
      }

      "doesn't list mined transactions from invalid range" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .flatMap(_.waitForDag())
          .runSyncUnsafe()

        val tx1 = valueTransfer(firstAccount.address, secondAccount.address, BigInt(10))
        val tx2 = valueTransfer(secondAccount.address, firstAccount.address, BigInt(42))

        val hash1 = service.personalSendTransaction(tx1, firstAccount.password).send() |> { res =>
          res.getTransactionHash
        }
        val hash2 = service.personalSendTransaction(tx2, secondAccount.password).send().getTransactionHash

        List(hash1, hash2).traverse(waitForTransaction).runSyncUnsafe()

        val currentBlock = service.ethGetBlockByNumber(latestBlock, false).send().getBlock.getNumber.asBigInt
        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, currentBlock + 2, currentBlock) |>
          (Await.result(_, defaultTimeout))

        val actualHashes: Set[String] = cursor
          .downField("result")
          .downField("transactions")
          .values
          .map(_.flatMap(transaction => {
            transaction.hcursor.downField("hash").as[String].toOption
          }))
          .getOrElse(Vector())
          .toSet

        actualHashes should be('empty)
      }

      "doesn't list contract transactions from invalid range" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .flatMap(_.waitForDag())
          .runSyncUnsafe()

        val tx1 = createContract(firstAccount.address, secondAccount.address)

        val hash = service.personalSendTransaction(tx1, firstAccount.password).send().getTransactionHash

        waitForTransaction(hash).runSyncUnsafe()

        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, 2, 0) |>
          (Await.result(_, defaultTimeout))

        val actualHashes: Set[String] = cursor
          .downField("result")
          .downField("transactions")
          .values
          .map(_.flatMap(transaction => {
            transaction.hcursor.downField("hash").as[String].toOption
          }))
          .getOrElse(Vector())
          .toSet

        actualHashes should be('empty)
      }

      "returns error if lower bound is lower than 0" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .flatMap(_.waitForDag())
          .runSyncUnsafe()

        val tx1 = valueTransfer(firstAccount.address, secondAccount.address, BigInt(10))
        val tx2 = valueTransfer(secondAccount.address, firstAccount.address, BigInt(42))

        val hash1 = service.personalSendTransaction(tx1, firstAccount.password).send() |> { res =>
          res.getTransactionHash
        }
        val hash2 = service.personalSendTransaction(tx2, secondAccount.password).send().getTransactionHash

        List(hash1, hash2).traverse(waitForTransaction).runSyncUnsafe()

        val currentBlock = service.ethGetBlockByNumber(latestBlock, false).send().getBlock.getNumber.asBigInt

        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, -2, currentBlock) |>
          (Await.result(_, defaultTimeout))

        cursor.downField("error").downField("code").as[Int] shouldBe Right(-32000)
      }

      "doesn't list contract transactions if lower bound is lower than 0" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .flatMap(_.waitForDag())
          .runSyncUnsafe()

        val tx1 = createContract(firstAccount.address, secondAccount.address)

        val hash = service.personalSendTransaction(tx1, firstAccount.password).send().getTransactionHash

        waitForTransaction(hash).runSyncUnsafe()

        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, -2, 2) |>
          (Await.result(_, defaultTimeout))

        cursor.downField("error").downField("code").as[Int] shouldBe Right(-32000)
      }

      "returns error if upper bound is lower than 0" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .flatMap(_.waitForDag())
          .runSyncUnsafe()

        val tx1 = valueTransfer(firstAccount.address, secondAccount.address, BigInt(10))
        val tx2 = valueTransfer(secondAccount.address, firstAccount.address, BigInt(42))

        val hash1 = service.personalSendTransaction(tx1, firstAccount.password).send() |> { res =>
          res.getTransactionHash
        }
        val hash2 = service.personalSendTransaction(tx2, secondAccount.password).send().getTransactionHash

        List(hash1, hash2).traverse(waitForTransaction).runSyncUnsafe()

        val currentBlock = service.ethGetBlockByNumber(latestBlock, false).send().getBlock.getNumber.asBigInt

        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, currentBlock, -2) |>
          (Await.result(_, defaultTimeout))

        cursor.downField("error").downField("code").as[Int] shouldBe Right(-32000)
      }

      "doesn't list contract transactions if upper bound is lower than 0" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .flatMap(_.waitForDag())
          .runSyncUnsafe()

        val tx1 = createContract(firstAccount.address, secondAccount.address)

        val hash = service.personalSendTransaction(tx1, firstAccount.password).send().getTransactionHash

        waitForTransaction(hash).runSyncUnsafe()

        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, 2, -2) |>
          (Await.result(_, defaultTimeout))

        cursor.downField("error").downField("code").as[Int] shouldBe Right(-32000)
      }

      "errors if range is bigger than 50 000" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningDisabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .runSyncUnsafe()

        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, 0, 50001) |>
          (Await.result(_, defaultTimeout))

        cursor.downField("error").downField("code").as[Int] shouldBe Right(-32602)
      }

      "errors if upper bound is missing" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningDisabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .runSyncUnsafe()

        val cursor = call("daedalus_getAccountTransactions", firstAccount.address, 0) |>
          (Await.result(_, defaultTimeout))

        cursor.downField("error").downField("code").as[Int] shouldBe Right(-32602)
      }

      "errors if bounds are missing" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningDisabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .runSyncUnsafe()

        val cursor = call("daedalus_getAccountTransactions", firstAccount.address) |>
          (Await.result(_, defaultTimeout))

        cursor.downField("error").downField("code").as[Int] shouldBe Right(-32602)
      }

      "errors if bounds and address are missing" in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningDisabled()
          .run(processes, defaultNetwork)
          .flatMap(_.waitForRpc())
          .runSyncUnsafe()

        val cursor = call("daedalus_getAccountTransactions") |> (Await.result(_, defaultTimeout))

        cursor.downField("error").downField("code").as[Int] shouldBe Right(-32602)
      }
    }
  }
}
