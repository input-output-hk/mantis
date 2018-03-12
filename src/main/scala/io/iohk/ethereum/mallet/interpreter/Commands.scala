package io.iohk.ethereum.mallet.interpreter

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.mallet.service.State
import io.iohk.ethereum.mallet.common.Util.OptionOps
import io.iohk.ethereum.rlp
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import org.spongycastle.util.encoders.Hex

import scala.reflect.ClassTag

object Commands {
  import AST._

  // TODO: support optional values
  case class Parameter(name: String, tpe: Class[_ <: Value])

  private def param[T <: Value : ClassTag](key: String): Parameter =
    Parameter(key, implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])

  sealed abstract class Command(val name: String, val parameters: Parameter*) {
    def run(state: State, arguments: Map[String, Value]): Result
  }

  object NewAccount extends Command("newAccount") {

    def run(state: State, arguments: Map[String, Value]): Result = {
      state.passwordReader.readPasswordTwice() match {
        case None =>
          Result("", state)

        case Some(password) =>
          state.keyStore.newAccount(password) match {
            case Left(err) =>
              Result(s"KeyStore error: $err", state)

            case Right(addr) =>
              Result(addr.toString, state)
          }
      }
    }
  }

  object ImportPrivateKey extends Command(
    "importPrivateKey",
    param[Number]("key")
  ) {

    def run(state: State, arguments: Map[String, Value]): Result = {
      state.passwordReader.readPasswordTwice() match {
        case None =>
          Result("", state)

        case Some(password) =>
          val key = arguments("key").asInstanceOf[Number].bytes
          state.keyStore.importPrivateKey(key, password) match {
            case Left(err) =>
              Result(s"KeyStore error: $err", state)

            case Right(addr) =>
              Result(addr.toString, state)
          }
      }
    }
  }

  object ListAccounts extends Command("listAccounts") {

    def run(state: State, arguments: Map[String, Value]): Result = {
      state.keyStore.listAccounts() match {
        case Right(addresses) =>
          val addressList = addresses.map(a => a.toString + (if (state.selectedAccount.contains(a)) " *" else ""))
          Result(addressList.mkString("\n"), state)

        case Left(err) =>
          Result(s"KeyStore error: $err", state)
      }
    }
  }

  object SelectAccount extends Command(
    "selectAccount",
    param[Number]("address")
  ) {

    def run(state: State, arguments: Map[String, Value]): Result = {
      val address = arguments("address").asInstanceOf[Number].address
      state.keyStore.listAccounts()
        .left.map(e => Result(s"KeyStore error: $e", state))
        .map { accounts =>
          if (accounts.contains(address))
            Result("", state.selectAccount(address))
          else
            Result(s"No account with address '$address' in KeyStore", state)
        }.merge
    }
  }

  // TODO: with expiration:
  // object UnlockAccount extends Command


  object SendTransaction extends Command(
    "sendTransaction",
    // TODO: optional parameters
    param[Number]("to"),
    param[Number]("gas"),
    param[Number]("gasPrice"),
    param[Number]("value"),
    param[Number]("data")
  ) {

    def run(state: State, arguments: Map[String, Value]): Result = {
      val (to, gas, gasPrice, value, data) = (
        Address(arguments("to").asInstanceOf[Number].n),
        arguments("gas").asInstanceOf[Number].n,
        arguments("gasPrice").asInstanceOf[Number].n,
        arguments("value").asInstanceOf[Number].n,
        arguments("data").asInstanceOf[Number].bytes
      )

      val result = for {
        from <- state.selectedAccount.toEither("No account selected")
        password <- state.passwordReader.readPassword().toEither("")
        wallet <- state.keyStore.unlockAccount(from, password).left.map(e => s"KeyStore error: $e")

        nonce <- state.rpcClient.getNonce(from).left.map(_.msg)

        tx = Transaction(nonce, gasPrice, gas, Some(to), value, data)
        stx = wallet.signTx(tx, None) // TODO: do we care about chainId
        bytes = ByteString(rlp.encode(stx.toRLPEncodable))

        txHash <- state.rpcClient.sendTransaction(bytes).left.map(_.msg)
      } yield Hex.toHexString(txHash.toArray)

      Result(result.merge, state)
    }
  }

  object GetBalance extends Command(
    "getBalance",
    param[Number]("address")
  ) {

    def run(state: State, arguments: Map[String, Value]): Result = {
      val address = arguments("address").asInstanceOf[Number].address
      state.rpcClient.getBalance(address)
        .left.map(e => Result(e.msg, state))
        .map(n => Result(n.toString, state))
        .merge
    }
  }

  object GetReceipt extends Command(
    "getReceipt",
    param[Number]("hash")
  ) {

    def run(state: State, arguments: Map[String, Value]): Result = {
      val hash = arguments("hash").asInstanceOf[Number].bytes
      state.rpcClient.getReceipt(hash) match {
        case Left(err) =>
          Result(err.msg, state)

        case Right(receipt) =>
          // TODO: format nicely
          Result(receipt.toString, state)
      }
    }
  }

}
