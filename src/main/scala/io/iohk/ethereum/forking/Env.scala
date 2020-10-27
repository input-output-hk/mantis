package io.iohk.ethereum.forking

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import cats.effect.Resource
import monix.eval.Task
import cats.implicits._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.forking.TxGenerator.Account

case class Env(
    actorSystem: ActorSystem,
    ethRpcClients: List[EthRpcClient],
    txGenerators: Task[List[TxGenerator]],
    forkChecker: ForkChecker
) {}
object Env {
  val actorSystemResource =
    Resource.make(Task { ActorSystem("forking_app") })(system => Task.deferFuture(system.terminate()).void)

  def apply(): Resource[Task, Env] =
    Env(Config.clientsMap, Config.accounts, Config.allReceivers)
  def apply(clientUris: Map[String, Uri], accounts: List[Account], receivers: List[Address]): Resource[Task, Env] = {
    for {
      system <- actorSystemResource
      ethRpcClients = clientUris.toList.map { case (name, uri) =>
        new EthRpcClient(name, uri)(system)
      }
      forkChecker = new ForkChecker(ethRpcClients)
      txGenerators = Config.accounts.zip(clientUris.values).traverse { case (account, uri) =>
        TxGenerator(account, receivers, new PersonalRpcClient(uri)(system))
      }
    } yield Env(system, ethRpcClients, txGenerators, forkChecker)
  }
}
