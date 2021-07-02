package io.iohk.ethereum.faucet.jsonrpc

import javax.net.ssl.SSLContext

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.util.ByteString

import monix.eval.Task

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

import io.circe.syntax._

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.client.RpcClient
import io.iohk.ethereum.jsonrpc.client.RpcClient.RpcError
import io.iohk.ethereum.security.SSLError
import io.iohk.ethereum.utils.Logger

class WalletRpcClient(node: Uri, timeout: Duration, getSSLContext: () => Either[SSLError, SSLContext])(implicit
    system: ActorSystem,
    ec: ExecutionContext
) extends RpcClient(node, timeout, getSSLContext)
    with Logger {
  import io.iohk.ethereum.jsonrpc.client.CommonJsonCodecs._

  def getNonce(address: Address): Task[Either[RpcError, BigInt]] =
    doRequest[BigInt]("eth_getTransactionCount", List(address.asJson, "latest".asJson))

  def sendTransaction(rawTx: ByteString): Task[Either[RpcError, ByteString]] =
    doRequest[ByteString]("eth_sendRawTransaction", List(rawTx.asJson))
}
