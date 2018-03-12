package io.iohk.ethereum.mallet.main

import java.security.SecureRandom
import java.time.Instant

import io.iohk.ethereum.keystore.KeyStoreImpl
import io.iohk.ethereum.mallet.interpreter.Interpreter
import io.iohk.ethereum.mallet.service.{RpcClient, State}

import scala.annotation.tailrec

object Mallet extends App {

  private val clOptions = OptionParser(args) match {
    case Some(co) => co
    case None => sys.exit(1)
  }

  private val shell = new Shell(clOptions.dataDir)

  private val initialState = {
    new State(
      shell,
      new RpcClient(clOptions.node),
      new KeyStoreImpl(clOptions.dataDir.getAbsolutePath, new SecureRandom()),
      None,
      None,
      Instant.now()
    )
  }


  @tailrec
  def loop(state: State): Unit = {

    shell.readLine() match {
      case Some(line) =>
        val result = Interpreter(line, state)
        shell.printLine(withNewLine(result.msg))
        loop(result.state)

      case None =>
        RpcClient.actorSystem.terminate()
    }
  }

  private def withNewLine(s: String): String = {
    val rightTrimmed = s.reverse.dropWhile(_.isWhitespace).reverse
    if (rightTrimmed.isEmpty) rightTrimmed else rightTrimmed + "\n"
  }

  loop(initialState)
}
