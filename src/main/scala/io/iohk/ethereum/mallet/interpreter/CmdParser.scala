package io.iohk.ethereum.mallet.interpreter

import io.iohk.ethereum.mallet.common.{Err, ParserError}
import AST._

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

/**
  * EBNF syntax definition:
  *
  * cmd             = identifier, { argument-list };
  * argument-list   = "(", (empty | argument, { ",",  argument }), ")";
  * argument        = named-argument | value;
  * named-argument  = identifier, "=", value;
  * value           = sequence | literal;
  * sequence        = "[", (empty | literal, { ",", literal }), "]";
  * literal         = quoted | dec | hex | identifier;
  * empty           = { whitespace };
  * identifier      = ? valid identifier ?;
  * dec             = ? decimal number ?;
  * hex             = 0x, ? hex digits ?;
  * quoted          = ? " delimited string ?;
  */
object CmdParser extends RegexParsers with JavaTokenParsers {

  val quoted: Parser[Quoted] = """\"([^"]|(?<=\\)")*\"""".r ^^ Quoted.apply

  val dec: Parser[Dec] = wholeNumber ^^ Dec.apply

  val hex: Parser[Hex] = "0x[A-Fa-f0-9]*".r ^^ Hex.apply

  val identifier: Parser[Identifier] = ident ^^ Identifier.apply

  val literal: Parser[Literal] = quoted | hex | dec | identifier

  val sequenceElems: Parser[List[Literal]] = literal ~ ("," ~> literal).* ^^ {
    case l ~ ls => l :: ls
  }

  val empty: Parser[List[Nothing]] = """\s*""".r ^^ (_ => Nil)

  val sequence: Parser[Sequence] = "[" ~> (sequenceElems | empty) <~ "]" ^^ Sequence.apply

  val value: Parser[Value] = sequence | literal

  val namedArgument: Parser[Argument] = (identifier <~ "=") ~ value ^^ {
    case name ~ v => Argument(Some(name.input), v)
  }

  val argument: Parser[Argument] = namedArgument | value ^^ (v => Argument(None, v))

  val argumentList: Parser[List[Argument]] = (argument ~ ("," ~> argument).* ^^ {
    case a ~ as => a :: as
  }) | empty


  val funCall: Parser[Cmd] = ident ~ ("(" ~> argumentList <~ ")").? ^^ {
    case name ~ arguments =>
      Cmd(name, arguments.getOrElse(Nil))
  }


  def apply(line: String): Either[Err, Cmd] = parseAll(funCall, line) match {
    case NoSuccess(message, _) =>
      val msg = if (line.trim.isEmpty) "" else message
      Left(ParserError(msg))

    case Success(matched, _) =>
      Right(matched)
  }
}
