package io.iohk.ethereum.mallet.interpreter

import akka.util.ByteString
import io.iohk.ethereum.domain.Address

object AST {

  case class Cmd(name: String, args: List[Argument])

  case class Argument(name: Option[String], value: Value)

  sealed trait Value

  case class QuotedString(s: String) extends Value

  case class Number(n: BigInt) extends Value {
    def bytes: ByteString = ByteString(n.toByteArray)
    def address: Address = Address(bytes)
  }

  case class Identifier(s: String) extends Value
}
