package io.iohk.ethereum.rpcTest
import java.math.BigInteger

import cats.effect.Resource
import cats.instances.list._
import cats.syntax.traverse._
import io.iohk.ethereum.rpcTest.data.Tags.{Eth, Standalone}
import io.iohk.ethereum.rpcTest.data.TestContracts._
import io.iohk.ethereum.rpcTest.data.TestData._
import io.iohk.ethereum.rpcTest.utils.ResourceOps._
import io.iohk.ethereum.rpcTest.utils.{MantisProcess, ProcessUtils, RpcTestConfigLoader}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import mouse.all._
import org.scalatest.{BeforeAndAfterEach, FreeSpec, Inspectors, Matchers}
import org.web3j.protocol.admin.Admin
import org.web3j.protocol.core.methods.request.Transaction
import org.web3j.protocol.core.methods.response.EthLog.Hash
import org.web3j.protocol.core.methods.response.{EthSendTransaction, EthSyncing}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.sys.process.Process

class EthRpcTests extends FreeSpec with Matchers with BeforeAndAfterEach with Inspectors with RpcTestConfigLoader {
  implicit val scheduler: Scheduler = monix.execution.Scheduler(scala.concurrent.ExecutionContext.global)
  val defaultTimeout: FiniteDuration = 1.minute
  val dagTimeout: FiniteDuration = 25.minutes
  val longTimeout = 20.minutes
  val defaultNetwork = "rpc-test-private"

  val allowPeersSettings = Map(
    "mantis.network.peer.max-incoming-peers" -> 4,
    "mantis.network.peer.max-pending-peers" -> 4,
  )

  var processes: mutable.Buffer[Process] = _

  override def beforeEach(): Unit =
    processes = mutable.ArrayBuffer()

  override def afterEach(): Unit = processes.toList.traverse(ProcessUtils.kill).runSyncUnsafe()

  "eth_" - {
    "syncing" - {
      val datadir = MantisProcess.getTempDatadir()

      val mantisForSyncing = MantisProcess
        .withTemporaryDatadir(datadir)
        .withPeersCommunication()
        .withDiscoveryEnabled()
        .withNetwork("eth")

      def waitForHigherBlock(service: Admin): Task[EthSyncing.Syncing] =
        Observable
          .interval(1.second)
          .map(_ => service.ethSyncing().send())
          .map(_.getResult)
          .collect {
            case m: EthSyncing.Syncing if utils.hexToBigInt(m.getCurrentBlock) > 0 => m
          }
          .firstL
          .delayResult(15.minutes)

      def waitForSyncing(service: Admin): Task[EthSyncing] =
        Observable
          .interval(1.second)
          .map(_ => service.ethSyncing().send())
          .filter(_.isSyncing)
          .firstL

      "started from empty dir should report 0 blocks" taggedAs (Standalone, Eth) in new ScenarioSetup {
        mantisForSyncing
          .run(proc => {
            proc
              .waitForRpc()
              .flatMap(_ => waitForSyncing(service))
              .map(_.getResult)
              .map {
                case m: EthSyncing.Syncing =>
                  hexToBigInt(m.getStartingBlock) shouldEqual 0
                  hexToBigInt(m.getCurrentBlock) should be >= BigInt(0)
                  hexToBigInt(m.getHighestBlock) should be > BigInt(0)
              }
              .flatMap(_ => waitForHigherBlock(service))
          })
          .runSyncUnsafe(longTimeout)
      }

      "started from non-empty dir should report blocks higher than 0" taggedAs (Standalone, Eth) in new ScenarioSetup {
        mantisForSyncing
          .run(proc => {
            proc
              .waitForRpc()
              .flatMap(_ => waitForSyncing(service))
              .map(_.getResult)
              .map {
                case m: EthSyncing.Syncing =>
                  hexToBigInt(m.getCurrentBlock) should be > BigInt(0)
                  hexToBigInt(m.getStartingBlock) should be > BigInt(0)
              }
          })
          .runSyncUnsafe(longTimeout)
      }
    }

    "coinbase" - {
      "should be picked from a parameter" taggedAs (Standalone, Eth) in new ScenarioSetup {
        private val coinbase = "9911223344556677889900112233445566778899"

        MantisProcess
          .withMiningEnabled()
          .withParam("mantis.consensus.coinbase", coinbase)
          .run(_.waitForRpc()
            .map(_ => service.ethCoinbase().send().getAddress shouldBe "0x" + coinbase))
          .runSyncUnsafe()
      }
    }

    "gasPrice" - {
      "should be 0 if no transactions are in blockchain" taggedAs (Standalone, Eth) in new ScenarioSetup {
        MantisProcess
          .withTemporaryDatadir()
          .withMiningEnabled()
          .run(_.waitForRpc().map(_ => {
            service.ethGasPrice().send().getGasPrice should equal(BigInteger.ZERO)
          }))
          .runSyncUnsafe()
      }

      "should be average of last 30 blocks if some transactions are in blockchain" taggedAs (Standalone, Eth) in new ScenarioSetup {
        def sendTransaction(tx: Transaction): Task[EthSendTransaction] =
          Task.delay(service.personalSendTransaction(tx, firstAccount.password).send())

        val transactions = (1 to 30).toList.map { i =>
          Transaction.createEtherTransaction(
            firstAccount.address,
            null,
            BigInt("20000000000") + i,
            null,
            secondAccount.address,
            BigInt(10))
        }

        def average(items: List[BigInt]): BigInt = items.sum / items.size

        val expectedAverage = transactions
          .map(_.getGasPrice.replace("0x", ""))
          .map(BigInt.apply(_, 16)) |> average |> BigIntToBingInteger

        MantisProcess
          .withMiningEnabled()
          .withTemporaryDatadir()
          .run(
            _.waitForRpc()
              .flatMap(_.waitForDag())
              .flatMap(_ => transactions.traverse(tx => sendTransaction(tx).map(_.getTransactionHash)))
              .flatMap(_.traverse(waitForTransaction))
              .map(_ => service.ethGasPrice().send().getGasPrice should equal(expectedAverage)))
          .runSyncUnsafe()
      }
    }

    "sendTransaction" - {
      "doesn't create transaction if miner is off" taggedAs (Standalone, Eth) in new ScenarioSetup {
        MantisProcess
          .withMiningDisabled()
          .run(
            _.waitForRpc()
              .flatMap(_ => {
                val tx = valueTransfer(firstAccount.address, secondAccount.address, BigInt(10))

                Task.delay(service.personalSendTransaction(tx, firstAccount.password).send().getTransactionHash)
              })
              .delayResult(2.minutes)
              .flatMap(hash => Task.delay(service.ethGetTransactionByHash(hash).send()))
              .map(res => res.getTransaction.isPresent should be(false)))
          .runSyncUnsafe()
      }
    }

    "getBlockByNumber" - {
      "returns error if second parameter is missing" taggedAs (Standalone, Eth) in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .create()
          .flatMap(proc => makeHttp().flatMap(http => Resource.pure(proc, http)))
          .use {
            case (proc, http) =>
              proc
                .waitForRpc()
                .flatMap(_ => http.call("eth_getBlockByNumber", "0x1"))
                .map(json => getErrorCode(json) shouldBe Right(-32602))
          }
          .runSyncUnsafe()
      }
    }

    "newBlockFilter" - {
      "works with fast sync" taggedAs (Standalone, Eth) in new ScenarioSetup {
        MantisProcess
          .withMiningDisabled()
          .withTemporaryDatadir()
          .withDiscoveryEnabled()
          .withFastSyncEnabled()
          .withNetwork("eth")
          .run(_.waitForRpc().flatMap(_ =>
            Task {
              val filterId = service.ethNewBlockFilter().send().getFilterId

              val importedBlock = service.blockObservable(false).toBlocking.first()

              val changes = service.ethGetFilterChanges(filterId).send()
              val logs = changes.getLogs.asScala.toList
              val addedBlocks = logs.map(log => log.asInstanceOf[Hash].get)

              addedBlocks should contain(importedBlock.getBlock.getHash)
          }))
          .runSyncUnsafe()
      }
    }

    "estimateGas" - {
      "estimates real usage of gas" taggedAs (Standalone, Eth) in new ScenarioSetup {
        val tx3 = createContract(firstAccount.address, testContract)

        val getUsage: Task[BigInt] = Task
          .delay(service.personalSendTransaction(tx3, firstAccount.password).send().getTransactionHash)
          .flatMap(waitForTransaction)
          .map(_.getGasUsed.toString |> BigInt.apply)

        MantisProcess
          .withMiningEnabled()
          .run(
            _.waitForRpc()
              .flatMap(_.waitForDag())
              .flatMap(_ => {
                for {
                  estimation <- Task.delay(service.ethEstimateGas(tx3).send().getAmountUsed).map(_.asBigInt)
                  usage <- getUsage
                } yield estimation shouldEqual usage
              }))
          .runSyncUnsafe()
      }
    }

    "sign" - {
      "work as other clients" taggedAs (Standalone, Eth) in new ScenarioSetup with RawHttpRpc with SigningSupport {
        def signString(str: String): Task[(String, String)] =
          Task {
            service.ethSign(accountWithKey.address, prepareForSign(str)).send()
          }.map(response => (str, response.getSignature))

        (MantisProcess.create(), makeHttp())
          .use2((proc, http) => {
            for {
              _ <- proc.waitForRpc()
              _ <- http.call("personal_importRawKey", keyForAccount, accountWithKey.password)
              _ <- Task { service.personalUnlockAccount(accountWithKey.address, accountWithKey.password, 60000).send() }
              results <- stringsToSign.traverse(signString)
            } yield forAll(results) { case (input, result) => result shouldBe signStr(input) }
          })
          .runSyncUnsafe()
      }

      "fails if account is not unlocked" taggedAs (Standalone, Eth) in new ScenarioSetup with SigningSupport {
        MantisProcess
          .run(proc =>
            for {
              _ <- proc.waitForRpc()
              res <- Task { service.ethSign(accountWithKey.address, prepareForSign("foo")).send() }
            } yield res.hasError shouldBe true)
          .runSyncUnsafe()
      }
    }

    "submitWork" - {
      "works with ethminer" taggedAs(Standalone, Eth) in new ScenarioSetup {
        val ethminerCommand = s"${testConfig.ethminerDir}/ethminer --opencl --pool ${testConfig.mantisUrl}"

        (MantisProcess
          .withMiningDisabled()
          .withTemporaryDatadir().create(), ProcessUtils.run(ethminerCommand)).use2((proc, _) => {
          for {
            _ <- proc.waitForRpc()
            res <- Task { service.blockObservable(false).toBlocking.first() }
          } yield res.getBlock.getNumber.asBigInt should be >= BigInt(1)
        }).runSyncUnsafe()
      }
    }
  }
}
