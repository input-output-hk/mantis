package io.iohk.ethereum.mallet.interpreter

import akka.util.ByteString
import io.circe.generic.auto._
import io.circe.syntax._
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.mallet.common.{StringUtil, Util}
import io.iohk.ethereum.mallet.interpreter.Parameter._
import io.iohk.ethereum.mallet.service.CommonJsonCodecs._
import io.iohk.ethereum.mallet.service.State
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.rlp
import org.bouncycastle.util.encoders.Hex


object Commands {

  /** Base class for all available commands */
  sealed abstract class Command(val name: String, val parameters: Parameter*) {
    protected val helpHeader: String
    protected val helpDetail: String

    lazy val help: String = {
      val optionalNote = if (parameters.exists(!_.required)) "(note: * - indicates optional parameter)\n" else ""
      val params = parameters.map {
        case Parameter(pName, tpe, required) =>
          val opt = if (required) "" else "*"
          s"$pName: $tpe$opt"
      }
      val signature = name + params.mkString("(", ", ", ")")

      s"""|$name: $helpHeader
          |usage:
          |  $signature
          |  $optionalNote
          |$helpDetail
      """.stripMargin
    }


    /** Runs the command given the state and an argument map. It depends on full prior validation of
      * of arguments and correctly built argument map (see: [[Interpreter]]). Will throw f the types of
      * the arguments do not match parameter expectations
      */
    def run(state: State, arguments: Map[String, Any]): Result
  }

  object NewAccount extends Command("newAccount") {

    def run(state: State, arguments: Map[String, Any]): Result = {
      state.passwordReader.readPasswordTwice() match {
        case None =>
          Result.error("Password not provided", state)

        case Some(password) =>
          state.keyStore.newAccount(password) match {
            case Left(err) =>
              Result.error(s"KeyStore error: $err", state)

            case Right(addr) =>
              Result.success(addr.toString, state)
          }
      }
    }

    val helpHeader: String = "creates new managed account"
    val helpDetail: String =
      """|Creates a new ECDSA key pair for a new managed account. A password will be required to store
         |the encrypted private key in Mallet's keystore
      """.stripMargin
  }

  object ImportPrivateKey extends Command(
    "importPrivateKey",
    required("key", Hash32)
  ) {

    def run(state: State, arguments: Map[String, Any]): Result = {
      state.passwordReader.readPasswordTwice() match {
        case None =>
          Result.error("Password not provided", state)

        case Some(password) =>
          val key = arguments("key").asInstanceOf[Hash32.T]
          state.keyStore.importPrivateKey(key, password) match {
            case Left(err) =>
              Result.error(s"KeyStore error: $err", state)

            case Right(addr) =>
              Result.success(addr.toString, state)
          }
      }
    }

    val helpHeader: String = "adds a managed account based on existing private key"
    val helpDetail: String =
      """|Accepts an existing ECDSA private key and adds a new managed account based on it. A password will be required to store
         |the encrypted private key in Mallet's keystore
      """.stripMargin
  }

  object ListAccounts extends Command("listAccounts") {

    def run(state: State, arguments: Map[String, Any]): Result = {
      state.keyStore.listAccounts() match {
        case Right(addresses) =>
          val addressList = addresses.map(a => a.toString + (if (state.selectedAccount.contains(a)) " *" else ""))
          Result.success(addressList.mkString("\n"), state)

        case Left(err) =>
          Result.error(s"KeyStore error: $err", state)
      }
    }

    val helpHeader: String = "lists managed accounts"
    val helpDetail: String = ""
  }

  object SelectAccount extends Command(
    "selectAccount",
    required("address", Addr20)
  ) {

    def run(state: State, arguments: Map[String, Any]): Result = {
      val address = arguments("address").asInstanceOf[Address]
      state.keyStore.listAccounts()
        .left.map(e => Result.error(s"KeyStore error: $e", state))
        .map { accounts =>
          if (accounts.contains(address))
            Result.success("", state.selectAccount(address))
          else
            Result.error(s"No account with address '$address' in KeyStore", state)
        }.merge
    }

    val helpHeader: String = "selects an account as a base for various commands"
    val helpDetail: String =
      """| Selects and account without unlocking it. The selected account will be used in various operations
         | like sending a transaction or checking account's balance
      """.stripMargin
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
        from <- state.selectedAccount.toRight("No account selected")
        password <- state.passwordReader.readPassword().toRight("Password not provided")
        wallet <- state.keyStore.unlockAccount(from, password).left.map(e => s"KeyStore error: $e")

        nonce <- state.rpcClient.getNonce(from).left.map(_.msg)

        tx = Transaction(nonce, gasPrice, gas, to, value, data.getOrElse(ByteString.empty))
        stx = wallet.signTx(tx, None) // TODO: do we care about chainId?
        bytes = ByteString(rlp.encode(stx.toRLPEncodable))

        txHash <- state.rpcClient.sendTransaction(bytes).left.map(_.msg)
      } yield StringUtil.prefix0x(Hex.toHexString(txHash.toArray))

      result match {
        case Left(msg) => Result.error(msg, state)
        case Right(msg) => Result.success(msg, state)
      }
    }

    val helpHeader: String = "send a transaction to be forged (mined)"
    val helpDetail: String =
      """|Create a transaction (TX) by specifying these parameters:
         |[to] - the TX recipient's address, if not provided it will be a contract creating TX
         |[gas] - gas amount available for contract execution
         |[gasPrice] - gas price per each unit of gas
         |[value] - value to transferred to the recipient (or to the newly created account)
         |[data] - the TX payload, empty if not provided
         |
         |For a TX to be created and signed, a signing account must be selected (see: 'selectAccount').
         |Upon successful sending of the TX, its 32-byte hash will be returned. This does not mean the TX has
         |already been forged.
      """.stripMargin
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
            .left.map(e => Result.error(e.msg, state))
            .map(n => Result.success(n.toString, state))
            .merge

        case None =>
          Result.error("No address provided or selected", state)
      }
    }

    val helpHeader: String = "get current funds of an account"
    val helpDetail: String =
      """|Provide [address] to get the balance of *any* account (the balance of a non-existent account will be zero)
         |or omit it to use currently selected account
      """.stripMargin
  }

  object GetReceipt extends Command(
    "getReceipt",
    required("hash", Hash32)
  ) {

    def run(state: State, arguments: Map[String, Any]): Result = {
      val hash = arguments("hash").asInstanceOf[Hash32.T]
      state.rpcClient.getReceipt(hash) match {
        case Left(err) =>
          Result.error(err.msg, state)

        case Right(None) =>
          Result.error("receipt not available", state)

        case Right(Some(receipt)) =>
          // TODO: format nicely (maybe JSON will do)
          Result.success(receipt.asJson.spaces4, state)
      }
    }

    val helpHeader = "fetch the receipt for a known transaction hash"
    val helpDetail =
      """|Shows the receipt of a transaction given its [hash]. The transaction must be already mined on the blockchain
      """.stripMargin
  }

  object Help extends Command(
    "help",
    optional("topic", Ident)
  ) {
    def run(state: State, arguments: Map[String, Any]): Result = {
      val topic = arguments("topic").asInstanceOf[Option[String]]
      val commands = Util.sealedDescendants[Command].map(cmd => cmd.name -> cmd).toMap

      topic match {
        case None =>
          val helpMsg =
            s"""|Available commands:
                |  ${commands.keys.toList.sorted.mkString("\n  ")}
                |
                |Use 'help([command])' for more information
            """.stripMargin
          Result.success(helpMsg, state)

        case Some(cmd) if commands.contains(cmd) =>
          Result.success(commands(cmd).help, state)

        case Some(invalid) =>
          Result.error(s"Invalid help topic: '$invalid'", state)
      }
    }

    val helpHeader: String = "display information about the app and its commands"
    val helpDetail: String =
      """|Use [topic] to display detailed information about a command. If [topic] is not provided
         |a list of available commands will be displayed
      """.stripMargin
  }

}
