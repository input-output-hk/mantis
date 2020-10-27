package io.iohk.ethereum.forking
import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, UInt256}
import io.iohk.ethereum.forking.RpcClient.RpcResponse
import io.iohk.ethereum.forking.TxGenerator.Account
import io.iohk.ethereum.utils.ByteStringUtils
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.duration.DurationInt
import scala.util.Random

class TxGenerator private (
    account: Account,
    otherAddresses: List[Address],
    personalEthRpcClient: PersonalRpcClient
) {
  val sentTransactions: Observable[ByteString] = Observable.repeatEvalF(for {
    receiver <- Task { Random.shuffle(otherAddresses).head }
    value <- Task { Random.nextLong().abs }.map(UInt256(_))
    gasPrice <- Task { Random.nextInt(100000).abs }.map(UInt256(_) + UInt256.One)
    txHash <- personalEthRpcClient
      .sendTransferTransaction(account.address, receiver, value, gasPrice, account.password)
      .peel
      .delayExecution(2.seconds)
  } yield txHash)
}
object TxGenerator {
  def apply(
      account: Account,
      otherAddresses: List[Address],
      personalEthRpcClient: PersonalRpcClient
  ): Task[TxGenerator] =
    (for {
      availableAccounts <- personalEthRpcClient.listAccounts()
      _ <-
        if (availableAccounts.contains(account.address)) RpcResponse.unit
        else personalEthRpcClient.importKey(account.privateKey, account.password)
      _ <- personalEthRpcClient.unlock(account.address, account.password)
    } yield new TxGenerator(account, otherAddresses, personalEthRpcClient)).peel

  case class Account(privateKey: ByteString, address: Address, password: String)
  object Account {
    def apply(privateKey: String, address: String, password: String): Account =
      Account(
        ByteStringUtils.string2hash(privateKey),
        Address(address),
        password
      )
  }
}
