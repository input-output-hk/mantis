package io.iohk.ethereum.mallet.interpreter

import akka.util.ByteString
import io.iohk.ethereum.mallet.common.StringUtil

/**
  * Holds classes that represent constructs and literal types parsed from user input
  */
object AST {

  case class Cmd(name: String, args: List[Argument])

  case class Argument(name: Option[String], value: Value)

  /**
    * Base trait for literal types of arguments from user input.
    * Note: these are different and require mapping to command parameter types
    */
  sealed trait Value {
    def input: String
  }

  sealed trait Literal extends Value

  case class Quoted(input: String) extends Literal {
    def unquote: String = StringUtil.unquote(input)
  }

  case class Dec(input: String) extends Literal {
    def number: BigInt = BigInt(input)
  }

  case class Hex(input: String) extends Literal {
    def bytes: ByteString = StringUtil.hexToBytes(input)
    def digits: String = StringUtil.drop0x(input)
    def number: BigInt = BigInt(digits, 16)
  }

  case class Identifier(input: String) extends Literal

  case class Sequence(elems: List[Literal]) extends Value {
    def input: String = elems.mkString("[", ",", "]")
  }

}
