package io.iohk.ethereum.rpcTest
import cats.effect.Resource
import io.circe.{Decoder, Json}
import io.iohk.ethereum.rpcTest.RawHttpRpc.HttpService
import io.iohk.ethereum.rpcTest.data.Tags.{Personal, Standalone}
import io.iohk.ethereum.rpcTest.data.TestData._
import io.iohk.ethereum.rpcTest.utils.{MantisProcess, RpcTestConfigLoader}
import monix.eval.Task
import org.scalatest.{FreeSpec, Inspectors, Matchers}
import utils.ResourceOps._
import cats.syntax.traverse._
import cats.instances.list._

import scala.concurrent.duration._

class PersonalRpcTests extends FreeSpec with Matchers with Inspectors with RpcTestConfigLoader {
  "personal_" - {
    "unlockAccount" - {
      "works with null send instead of duration" taggedAs (Standalone, Personal) in new ScenarioSetup with RawHttpRpc {
        MantisProcess
          .create()
          .flatMap(proc => makeHttp().flatMap(http => Resource.pure((proc, http))))
          .use {
            case (proc, http) =>
              proc
                .waitForRpc()
                .flatMap(_ => http.call("personal_unlockAccount", firstAccount.address, firstAccount.password, null))
                .map(json => {
                  getResult[Boolean](json) shouldBe Right(true)
                })
          }
          .runSyncUnsafe()
      }

      "should unlock account for 5 minutes if no duration is given" taggedAs(Standalone, Personal) in new ScenarioSetup {
        val tx = valueTransfer(firstAccount.address, secondAccount.address, BigInt(10))

        val lockAccount = Task { service.personalUnlockAccount(firstAccount.address, firstAccount.password).send() }

        val sendTx = Task { service.ethSendTransaction(tx).send() }

        MantisProcess
          .withTemporaryDatadir()
          .run({
            _.waitForRpc()
              .flatMap(_ => lockAccount)
              .flatMap(_ => sendTx)
              .map(res => res.getTransactionHash should not be null)
              .delayResult(5.minutes)
              .flatMap(_ => sendTx)
              .map(res => res.hasError shouldBe true)
          })
          .runSyncUnsafe()
      }
    }

    "importRawKey" - {
      def importKey(http: HttpService): Task[Json] =
        http.call("personal_importRawKey", keyForAccount, accountWithKey.password)

      "should return an address when called first time" taggedAs (Standalone, Personal) in new ScenarioSetup with RawHttpRpc {
        (MantisProcess.withTemporaryDatadir().create(), makeHttp())
          .use2((proc, http) => {
              for {
                _ <- proc.waitForRpc()
                res <- importKey(http)
              } yield getResult[String](res) shouldBe Right(accountWithKey.address)
          })
          .runSyncUnsafe()
      }

      "should return an error when called multiple times" taggedAs (Standalone, Personal) in new ScenarioSetup with RawHttpRpc {
        (MantisProcess.withTemporaryDatadir().create(), makeHttp())
          .use2((proc, http) =>
              for {
                _ <- proc.waitForRpc()
                _ <- importKey(http)
                res <- importKey(http)
              } yield getErrorCode(res) shouldBe Right(-32000)
          )
          .runSyncUnsafe()
      }
    }

    "lockAccount" - {
      "should make it impossible to send a transaction" taggedAs (Standalone, Personal) in new ScenarioSetup with RawHttpRpc {
        val tx = valueTransfer(firstAccount.address, secondAccount.address, BigInt(10))

        (MantisProcess.withMiningEnabled().withTemporaryDatadir().create(), makeHttp())
          .use2((proc, http) =>
            for {
              _ <- proc.waitForRpc()
              _ <- Task.delay(service.personalUnlockAccount(firstAccount.address, firstAccount.password).send())
              _ <- http.call("personal_lockAccount", firstAccount.address)
              res <- Task.delay(service.ethSendTransaction(tx).send())
            } yield res.hasError shouldBe true)
          .runSyncUnsafe()
      }
    }

    "sign" - {
      "allows to pass a password instead of unlocking account upfront"
        .taggedAs(Standalone, Personal)
        .in(new ScenarioSetup with RawHttpRpc with SigningSupport {
          (MantisProcess.create(), makeHttp()).use2((proc, http) => {
            def signString(str: String): Task[(String, Decoder.Result[String])] =
              http
                .call("personal_sign", prepareForSign(str), accountWithKey.address, accountWithKey.password)
                .map(res => (str, getResult[String](res)))

            for {
              _ <- proc.waitForRpc()
              _ <- http.call("personal_importRawKey", keyForAccount, accountWithKey.password)
              results <- stringsToSign.traverse(signString)
            } yield forAll(results) {
              case (input, result) => result shouldBe Right(signStr(input))
            }
          }).runSyncUnsafe()
      })

      "errors if password is not passed" taggedAs (Standalone, Personal) in new ScenarioSetup with RawHttpRpc with SigningSupport {
        (MantisProcess.create(), makeHttp()).use2((proc, http) => {
          for {
            _ <- proc.waitForRpc()
            _ <- http.call("personal_importRawKey", keyForAccount, accountWithKey.password)
            res <- http.call("personal_sign", prepareForSign("foo"), accountWithKey.address)
          } yield getErrorCode(res) shouldBe Right(-32602)
        }).runSyncUnsafe()
      }
    }

    "ecRecover" - {
      "is complementary to signing" taggedAs(Standalone, Personal) in new ScenarioSetup with RawHttpRpc with SigningSupport {
        (MantisProcess.create(), makeHttp()).use2((proc, http) => {
          def signAndRecover(str: String): Task[Decoder.Result[String]] = {
            val prepared  = prepareForSign(str)

            Task { service.ethSign(firstAccount.address, prepared).send().getSignature}
              .flatMap(signature => http.call("personal_ecRecover", prepared, signature))
              .map(getResult[String])
          }

          for {
            _ <- proc.waitForRpc()
            _ <- Task { service.personalUnlockAccount(firstAccount.address, firstAccount.password, 100000).send() }
            results <- stringsToSign.traverse(signAndRecover)
          } yield forAll(results)(_ shouldBe Right(firstAccount.address))
        }).runSyncUnsafe()
      }
    }
  }
}
