package io.iohk.ethereum.mallet.interpreter

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.mallet.interpreter.AST._
import io.iohk.ethereum.mallet.interpreter.Parameter.ParamType

object Parameter {
  sealed trait ParamType {
    type T

    protected def convert: PartialFunction[Literal, T]

    protected def errorHint: PartialFunction[Literal, String] = PartialFunction.empty

    protected def errorMsg(value: Literal): String =
      s"cannot interpret '${value.input}' as $this"

    def fromLiteral(value: Literal): Either[String, T] = {
      val tryConvert =
        convert.andThen(Right(_))
          .orElse(errorHint.andThen(s => Left(errorMsg(value) + s" ($s)")))

      tryConvert.applyOrElse(value, (_: Any) => Left(errorMsg(value)))
    }
  }

  case object Number extends ParamType {
    type T = BigInt

    protected val convert = {
      case d: Dec => d.number
      case h: Hex if h.digits.nonEmpty => h.number
    }
  }

  case object CharSeq extends ParamType {
    type T = String

    protected val convert = {
      case q: Quoted => q.unqoute
    }
  }

  case object Bytes extends ParamType {
    type T = ByteString

    protected val convert = {
      case h: Hex => h.bytes
      case q: Quoted => ByteString(q.unqoute)
    }
  }

  case object Addr20 extends ParamType {
    type T = Address

    protected val convert = {
      case h: Hex if h.bytes.nonEmpty && h.bytes.length <= 20 =>
        Address(h.bytes)
    }

    override protected val errorHint = {
      case Hex(s) =>
        "value needs to be non-empty and at most 20 bytes"
    }
  }

  case object Hash32 extends ParamType {
    type T = ByteString

    protected val convert = {
      case h: Hex if h.bytes.length == 32 =>
        h.bytes
    }

    override protected val errorHint = {
      case h: Hex =>
        "value needs to exactly 32 bytes"
    }
  }

  case object Ident extends ParamType {
    type T = String

    protected val convert = {
      case Identifier(s) => s
    }
  }


  def required(key: String, tpe: ParamType): Parameter =
    Parameter(key, tpe, required = true)

  def optional(key: String, tpe: ParamType): Parameter =
    Parameter(key, tpe, required = false)
}

case class Parameter(name: String, tpe: ParamType, required: Boolean)

