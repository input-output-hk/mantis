package io.iohk.ethereum.rpc

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import io.iohk.ethereum.blockchain.Blockchain
import io.iohk.ethereum.utils.Config

object JsonRpcServer {

  def run(implicit actorSystem: ActorSystem, blockchain: Blockchain): Unit = {
    import Config.Network.Rpc._

    implicit val materializer = ActorMaterializer()
    Http().bindAndHandle(BlockController.route(blockchain), interface, port)
  }
}
