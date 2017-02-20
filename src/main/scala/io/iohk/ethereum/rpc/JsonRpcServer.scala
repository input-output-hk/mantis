package io.iohk.ethereum.rpc

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.utils.Config

trait RpcServerConfig {
  val enabled: Boolean
  val interface: String
  val port: Int
}

object JsonRpcServer {

  def run(implicit actorSystem: ActorSystem, blockchain: Blockchain, config: RpcServerConfig): Unit = {
    implicit val materializer = ActorMaterializer()
    Http().bindAndHandle(BlockController.route(blockchain), config.interface, config.port)
  }
}
