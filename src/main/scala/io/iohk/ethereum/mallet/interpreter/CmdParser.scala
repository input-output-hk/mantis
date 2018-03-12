package io.iohk.ethereum.mallet.interpreter

import io.iohk.ethereum.mallet.common.{Err, ParserError}
import AST._

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

/**
  * EBNF:
  *
  * cmd             = identifier, { argument-list };
  * argument-list   = "(", (empty | argument, { ",",  argument }), ")";
  * argument        = named-argument | value;
  * named-argument  = ident, "=", value;
  * value           = quoted | number | identifier;
  * empty           = { whitespace };
  * identifier      = ? valid identifier ?;
  * number          = ? decimal or hexadecimal number ?;
  * quoted          = ? " delimited string ?;
  */
object CmdParser extends RegexParsers with JavaTokenParsers {

  val string: Parser[String] = "\"" ~> """([^"]|(?<=\\)")*""".r <~ "\""

  val decNumber: Parser[BigInt] = wholeNumber ^^ BigInt.apply

  val hexNumber: Parser[BigInt] = ("0x" ~> "[A-Fa-f0-9]+".r) ^^ {
    digits => BigInt(digits, 16)
  }

  val number: Parser[Number] = (hexNumber | decNumber) ^^ Number.apply

  val identifier: Parser[Identifier] = ident ^^ Identifier.apply

  val quotedString: Parser[QuotedString] = string ^^ QuotedString.apply

  val value: Parser[Value] = number | identifier | quotedString

  val namedArgument: Parser[Argument] = (identifier <~ "=") ~ value ^^ {
    case name ~ v => Argument(Some(name.s), v)
  }

  val argument: Parser[Argument] = namedArgument | value ^^ (v => Argument(None, v))

  val empty: Parser[List[Nothing]] = """\s*""".r ^^ (_ => Nil)

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
