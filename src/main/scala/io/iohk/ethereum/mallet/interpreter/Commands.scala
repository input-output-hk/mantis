package io.iohk.ethereum.mallet.interpreter

import akka.util.ByteString
import io.iohk.ethereum.domain.Transaction
import io.iohk.ethereum.mallet.common.Util
import io.iohk.ethereum.mallet.common.Util.OptionOps
import io.iohk.ethereum.mallet.interpreter.Parameter._
import io.iohk.ethereum.mallet.service.State
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.rlp
import org.spongycastle.util.encoders.Hex


object Commands {

  sealed abstract class Command(val name: String, val parameters: Parameter*) {
    def run(state: State, arguments: Map[String, Any]): Result
  }

  object NewAccount extends Command("newAccount") {

    def run(state: State, arguments: Map[String, Any]): Result = {
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
    required("key", Hash32)
  ) {

    def run(state: State, arguments: Map[String, Any]): Result = {
      state.passwordReader.readPasswordTwice() match {
        case None =>
          Result("", state)

        case Some(password) =>
          val key = arguments("key").asInstanceOf[Hash32.T]
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

    def run(state: State, arguments: Map[String, Any]): Result = {
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
    required("address", Addr20)
  ) {

    def run(state: State, arguments: Map[String, Any]): Result = {
      val address = arguments("address").asInstanceOf[Addr20.T]
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
    optional("to", Addr20),
    required("gas", Number),
    required("gasPrice", Number),
    required("value", Number),
    optional("data", Bytes)
  ) {

    def run(state: State, arguments: Map[String, Any]): Result = {
      val (to, gas, gasPrice, value, data) = (
        arguments("to").asInstanceOf[Option[Addr20.T]],
        arguments("gas").asInstanceOf[Number.T],
        arguments("gasPrice").asInstanceOf[Number.T],
        arguments("value").asInstanceOf[Number.T],
        arguments("data").asInstanceOf[Option[Bytes.T]]
      )

      val result = for {
        from <- state.selectedAccount.toEither("No account selected")
        password <- state.passwordReader.readPassword().toEither("")
        wallet <- state.keyStore.unlockAccount(from, password).left.map(e => s"KeyStore error: $e")

        nonce <- state.rpcClient.getNonce(from).left.map(_.msg)

        tx = Transaction(nonce, gasPrice, gas, to, value, data.getOrElse(ByteString.empty))
        stx = wallet.signTx(tx, None) // TODO: do we care about chainId
        bytes = ByteString(rlp.encode(stx.toRLPEncodable))

        txHash <- state.rpcClient.sendTransaction(bytes).left.map(_.msg)
      } yield Hex.toHexString(txHash.toArray)

      Result(result.merge, state)
    }
  }

  object GetBalance extends Command(
    "getBalance",
    optional("address", Addr20)
  ) {

    def run(state: State, arguments: Map[String, Any]): Result = {
      val addressOpt = arguments("address").asInstanceOf[Option[Addr20.T]] orElse state.selectedAccount

      addressOpt match {
        case Some(address) =>
          state.rpcClient.getBalance(address)
            .left.map(e => Result(e.msg, state))
            .map(n => Result(n.toString, state))
            .merge

        case None =>
          Result("No address provided or selected", state)
      }
    }
  }

  object GetReceipt extends Command(
    "getReceipt",
    required("hash", Hash32)
  ) {

    def run(state: State, arguments: Map[String, Any]): Result = {
      val hash = arguments("hash").asInstanceOf[Hash32.T]
      state.rpcClient.getReceipt(hash) match {
        case Left(err) =>
          Result(err.msg, state)

        case Right(receipt) =>
          // TODO: format nicely
          Result(receipt.toString, state)
      }
    }
  }

  object Help extends Command(
    "help"
  ) {
    def run(state: State, arguments: Map[String, Any]): Result = {
      val commands = Util.sealedDescendants[Command].toList.map(_.name).sorted
      Result(commands.mkString("\n"), state)
    }
  }

}
