package io.iohk.ethereum.mallet.interpreter

import io.iohk.ethereum.mallet.common.Util
import io.iohk.ethereum.mallet.interpreter.AST._
import io.iohk.ethereum.mallet.interpreter.Commands.Command
import io.iohk.ethereum.mallet.service.State

object Interpreter {

  val commands: Map[String, Command] =
    Util.sealedDescendants[Command].toList.map(c => c.name -> c).toMap

  def apply(input: String, state: State): Result = {
    CmdParser(input)
      .left.map(_.msg)
      .flatMap(validateCmd)
      .map(runCmd(state))
      .left.map(Result(_, state))
      .merge
  }

  private def validateCmd(cmd: Cmd): Either[String, Cmd] = {
    lazy val validCmd = commands.isDefinedAt(cmd.name)
    lazy val argsNamed = cmd.args.forall(_.name.isDefined)
    lazy val uniformArgs = cmd.args.forall(_.name.isEmpty) || argsNamed
    lazy val cmdObj = commands(cmd.name)

    if (!validCmd)
      Left(s"Unknown command: ${cmd.name}")

    else if (!uniformArgs)
      Left("Mixing named and non-named arguments is not supported")

    // TODO: support optional values
    else if (cmd.args.length < cmdObj.parameters.length)
      Left(s"Too few arguments for: ${cmd.name}")

    else if (!argsNamed) {
      val named = cmdObj.parameters.map(_.name).zip(cmd.args).map {
        case (name, arg) => Argument(Some(name), arg.value)
      }
      Right(Cmd(cmd.name, named.toList)).flatMap(validateArgs)
    }

    else
      Right(cmd).flatMap(validateArgs)
  }

  private def validateArgs(cmd: Cmd): Either[String, Cmd] = {
    val argMap = cmd.args.map { case Argument(name, value) => name.get -> value }.toMap
    val argTypes = commands(cmd.name).parameters.map(p => (p, argMap.get(p.name).map(_.getClass)))

    argTypes.collectFirst {
      case (param, Some(argType)) if param.tpe != argType =>
        Left(s"Invalid argument type for '${param.name}', got: ${argType.getSimpleName}, expected: ${param.tpe.getSimpleName}")

      case (param, None) =>
        Left(s"Missing argument for '${param.name}'")
    }.getOrElse(Right(cmd))
  }

  private def runCmd(state: State)(cmd: Cmd): Result = {
    val argMap = cmd.args.map { case Argument(name, value) => name.get -> value }.toMap
    commands(cmd.name).run(state, argMap)
  }
}
