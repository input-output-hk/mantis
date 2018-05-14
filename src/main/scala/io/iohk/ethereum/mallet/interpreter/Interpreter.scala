package io.iohk.ethereum.mallet.interpreter

import io.iohk.ethereum.mallet.common.Util
import io.iohk.ethereum.mallet.interpreter.AST._
import io.iohk.ethereum.mallet.interpreter.Commands.Command
import io.iohk.ethereum.mallet.service.State

object Interpreter {

  val commands: Map[String, Command] =
    Util.sealedDescendants[Command].toList.map(c => c.name -> c).toMap

  /**
    * Attempts to parse and run user's input against the current state
    */
  def apply(input: String, state: State): Result = {
    val result = for {
      cmd <- CmdParser(input).left.map(err => Result.error(err.msg, state))
      validCmd <- validateCmd(cmd).left.map(Result.error(_, state))
    } yield buildArgsAndRun(validCmd, state)

    result.merge
  }

  /**
    * Basic validation of grammatically well-formed user input
    */
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

  /**
    * Used when command arguments are not named - it resolves parameters which have been provided taking into
    * account optional parameters.
    * Optional parameters are matched with positional arguments as long as the number of arguments satisfies all
    * required parameters.
    * This function depends on the prior validation of the number of arguments.
    */
  private def matchProvidedParams(allParams: List[Parameter], providedNum: Int): List[Parameter] = {
    val requiredParams = allParams.filter(_.required)
    if (providedNum == 0)
      Nil
    else if (providedNum == requiredParams.length)
      requiredParams
    else
      allParams.head :: matchProvidedParams(allParams.tail, providedNum - 1)
  }

  /**
    * Attempts to build a map of command arguments, mapping literal types to parameters types,
    * and if successful, runs the command
    * Note: arguments for optional parameters, are always wrapped in Option
    */
  private def buildArgsAndRun(cmd: Cmd, state: State): Result = {
    val cmdObj = commands(cmd.name)
    val literalArgs = cmd.args.map(a => a.name.get -> a.value).toMap

    val zero: Either[Result, Map[String, Any]] = Right(Map())

    val argValueMap = cmdObj.parameters.foldLeft(zero) {
      case (Right(m), Parameter(name, tpe, required)) =>

        val value: Either[String, Option[Any]] =
          literalArgs.get(name)
            .map(a => tpe.fromValue(a).map(Some(_)))
            .getOrElse(Right(None))

        value.map {
          case Some(v) if required => m + (name -> v)
          case other => m + (name -> other)
        }.left.map(Result.error(_, state))

      case (left, _) =>
        left
    }

    argValueMap.map(cmdObj.run(state, _)).merge
  }
}
