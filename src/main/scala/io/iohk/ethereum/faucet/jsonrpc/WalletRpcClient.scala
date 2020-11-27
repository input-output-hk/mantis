package io.iohk.ethereum.faucet.jsonrpc

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import io.circe.syntax._
import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.client.RpcClient
import io.iohk.ethereum.jsonrpc.client.RpcClient.RpcError
import io.iohk.ethereum.security.SSLError
import io.iohk.ethereum.utils.Logger
import javax.net.ssl.SSLContext
import monix.eval.Task

import scala.concurrent.ExecutionContext

class WalletRpcClient(node: Uri, fSslContext: () => Either[SSLError, SSLContext])(implicit
    system: ActorSystem,
    ec: ExecutionContext
) extends RpcClient(node, fSslContext)
    with Logger {
  import io.iohk.ethereum.jsonrpc.client.CommonJsonCodecs._

  def getNonce(address: Address): Task[Either[RpcError, BigInt]] =
    doRequest[BigInt]("eth_getTransactionCount", List(address.asJson, "latest".asJson))

  def sendTransaction(rawTx: ByteString): Task[Either[RpcError, ByteString]] =
    doRequest[ByteString]("eth_sendRawTransaction", List(rawTx.asJson))
}
