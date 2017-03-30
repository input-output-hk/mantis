package io.iohk.ethereum.vmrunner

import io.iohk.ethereum.vmrunner.AST.FunCall

sealed trait RunnerError {
  def msg: String
}

case class ParserError(msg: String) extends RunnerError

case class InvalidProperty(name: String) extends RunnerError {
  val msg = s"Invalid property: $name"
}

case class InvalidAttribute(name: String) extends RunnerError {
  val msg = s"Invalid attribute: $name"
}

case class UnknownAccount(name: String) extends RunnerError {
  val msg = s"Uknown account: $name"
}

case class UnknownFunction(call: FunCall) extends RunnerError {
  val msg = s"Uknown function: ${call.name} on contract: ${call.target}"
}

case class InvalidFunctionInvocation(call: FunCall, expected: String) extends RunnerError {
  val msg = s"Invalid function call: ${call.name}(${call.args.mkString(",")} on contract: ${call.target}. Expected: $expected)"
}

case class ContractFileNotFound(name: String, tpe: String) extends RunnerError {
  val msg = s"$tpe file for contract $name not found"
}

case class JsonError(errMsg: String) extends RunnerError {
  val msg = s"Could not parse ABI file: $errMsg"
}

case object UnknownError extends RunnerError {
  val msg = "Unknown error"
}
