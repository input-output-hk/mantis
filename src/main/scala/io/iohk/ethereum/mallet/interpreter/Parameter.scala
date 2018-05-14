package io.iohk.ethereum.mallet.interpreter

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.mallet.interpreter.AST._
import io.iohk.ethereum.mallet.interpreter.Parameter.ParamType

object Parameter {

  /**
    * Base trait for command parameter types, which define mappings from literal types (from user input)
    */
  sealed trait ParamType {
    /** Scala type used as a representation of parameter type */
    type T

    /** Defines matching conversions from literal type to parameter type */
    protected def convert: PartialFunction[Value, T]

    /** Used to provided additional information on why the conversion failed (e.g. byte sequence of invalid length) */
    protected def errorHint: PartialFunction[Value, String] = PartialFunction.empty

    protected def errorMsg(value: Value): String = {
      val hint = errorHint.andThen("(" + _ + ")").applyOrElse(value, (_: Any) => "")
      s"cannot interpret '${value.input}' as $this $hint"
    }

    /** text representation of the type (e.g. in help strings) */
    def show: String = toString

    /** Attempts to convert value type to parameter type, and provides error message if failed */
    def fromValue(value: Value): Either[String, T] = {
      val tryConvert = convert.andThen(Right(_))
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
      case q: Quoted => q.unquote
    }
  }

  case object Bytes extends ParamType {
    type T = ByteString

    protected val convert = {
      case d: Dec => ByteString(d.number.toByteArray)
      case h: Hex => h.bytes
      case q: Quoted => ByteString(q.unquote)
    }
  }

  case object Addr20 extends ParamType {
    type T = Address

    protected val convert = {
      // TODO: consider conversion from a quoted string which is common in other clients,
      // on the other hand it relaxes the syntax and may be a problem when adding alternative types for functions
      // (e.g. referring to an account by alias)
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
        "value needs to be exactly 32 bytes"
    }
  }

  case object Ident extends ParamType {
    type T = String

    protected val convert = {
      case Identifier(s) => s
    }
  }

  case class Multiple(tpe: ParamType) extends ParamType {
    type T = List[tpe.T]

    protected val convert = PartialFunction.empty

    override def fromValue(value: Value): Either[String, T] = value match {
      case Sequence(xs) =>
        val z: Either[String, List[tpe.T]] = Right(Nil)
        xs.map(tpe.fromValue).foldLeft(z) { (acc, x) =>
          for (ys <- acc; y <- x) yield y :: ys
        }.map(_.reverse)


      case other =>
        Left(errorMsg(other))
    }

    override def show: String = s"[${tpe.show}]"
  }


  def required(key: String, tpe: ParamType): Parameter =
    Parameter(key, tpe, required = true)

  def optional(key: String, tpe: ParamType): Parameter =
    Parameter(key, tpe, required = false)
}

case class Parameter(name: String, tpe: ParamType, required: Boolean)

