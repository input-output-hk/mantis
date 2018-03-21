package io.iohk.ethereum.mallet.interpreter

import akka.util.ByteString
import io.iohk.ethereum.mallet.common.StringUtil._

object AST {

  case class Cmd(name: String, args: List[Argument])

  case class Argument(name: Option[String], value: Literal)

  sealed trait Literal {
    def input: String
  }

  case class Quoted(input: String) extends Literal {
    def unqoute: String = unquote(input)
  }

  case class Dec(input: String) extends Literal {
    def number: BigInt = BigInt(input)
  }

  case class Hex(input: String) extends Literal {
    def bytes: ByteString = hexToBytes(input)
    def digits: String = drop0x(input)
    def number: BigInt = BigInt(digits, 16)
  }

  case class Identifier(input: String) extends Literal

}
