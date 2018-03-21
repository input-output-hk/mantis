package io.iohk.ethereum.mallet.interpreter

import io.iohk.ethereum.mallet.common.Util
import io.iohk.ethereum.mallet.interpreter.AST._
import io.iohk.ethereum.mallet.interpreter.Commands.Command
import io.iohk.ethereum.mallet.service.State

object Interpreter {

  val commands: Map[String, Command] =
    Util.sealedDescendants[Command].toList.map(c => c.name -> c).toMap

  def apply(input: String, state: State): Result = {
    val result = for {
      cmd <- CmdParser(input).left.map(err => Result(err.msg, state))
      validCmd <- validateCmd(cmd).left.map(Result(_, state))
    } yield buildArgsAndRun(validCmd, state)

    result.merge
  }

  private def validateCmd(cmd: Cmd): Either[String, Cmd] = {
    lazy val validCmd = commands.isDefinedAt(cmd.name)
    lazy val argsNamed = cmd.args.forall(_.name.isDefined)
    lazy val uniformArgs = cmd.args.forall(_.name.isEmpty) || argsNamed
    lazy val cmdObj = commands(cmd.name)
    lazy val missingRequiredArg =
      cmdObj.parameters.filter(_.required)
        .find(p => cmd.args.forall(!_.name.contains(p.name)))
        .map(_.name)

    if (!validCmd)
      Left(s"Unknown command: ${cmd.name}")

    else if (!uniformArgs)
      Left("Mixing named and non-named arguments is not supported")

    else if (cmd.args.length < cmdObj.parameters.count(_.required))
      Left(s"Too few arguments for: ${cmd.name}")

    else if (cmd.args.length > cmdObj.parameters.length)
      Left(s"Too many arguments for: ${cmd.name}")

    else if (!argsNamed) {
      val providedParams = matchProvidedParams(cmdObj.parameters.toList, cmd.args.length)
      val names = providedParams.map(p => Some(p.name))
      val values = cmd.args.map(_.value)
      val namedArgs = names.zip(values).map(Argument.tupled)
      Right(cmd.copy(args = namedArgs))
    }

    else if (missingRequiredArg.isDefined)
      Left(s"No value provided for parameter '${missingRequiredArg.get}'")

    else
      Right(cmd)
  }

  private def matchProvidedParams(allParams: List[Parameter], providedNum: Int): List[Parameter] = {
    val requiredParams = allParams.filter(_.required)
    if (providedNum == 0)
      Nil
    else if (providedNum == requiredParams.length)
      requiredParams
    else
      allParams.head :: matchProvidedParams(allParams.tail, providedNum - 1)
  }

  private def buildArgsAndRun(cmd: Cmd, state: State): Result = {
    val cmdObj = commands(cmd.name)
    val literalArgs = cmd.args.map(a => a.name.get -> a.value).toMap

    val zero: Either[Result, Map[String, Any]] = Right(Map())
    val argValueMap = cmdObj.parameters.foldLeft(zero) {
      case (Right(m), Parameter(name, tpe, required)) =>
        val value = literalArgs.get(name).map(tpe.fromLiteral).getOrElse(Right(None))
        value.map(v => if (required) v else Some(v)).map(v => m + (name -> v))
          .left.map(Result(_, state))

      case (left, _) =>
        left
    }

    argValueMap.map(cmdObj.run(state, _)).merge
  }
}
