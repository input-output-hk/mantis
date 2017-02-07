package io.iohk.ethereum.vmrunner

import io.iohk.ethereum.vmrunner.AST._

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

/**
  * EBNF:
  *
  * cmd         = create | accounts | fun-call | property | object | do-nothing;
  * create      = "create", {attr}, "(", ident, ")";
  * accounts    = "accounts";
  * fun-call    = ident, ".", ident, {attr}, "(", arg, {",", arg}, ")";
  * property    = ident, ".", ident;
  * object      = ident;
  * do-nothing  = ? empty string or whitespaces ?;
  * attr        = ".", ident, "(", number, ")";
  * arg         = number | string;
  * ident       = ? valid identifier ?;
  * number      = ? decimal or hexadecimal number ?;
  * string      = ? " delimited string ?;
  */
object CmdParser extends RegexParsers with JavaTokenParsers {

  val string: Parser[String] = "\"" ~> """([^"]|(?<=\\)")*""".r <~ "\""

  val decNumber: Parser[BigInt] = wholeNumber ^^ BigInt.apply

  val hexNumber: Parser[BigInt] = ("-".? ~ ("0x" ~> "[A-Fa-f0-9]+".r)) ^^ {
    case sgn ~ digits => BigInt(sgn.getOrElse("") + digits, 16)
  }

  val number: Parser[BigInt] = hexNumber | decNumber

  val arg: Parser[String] = string | number ^^ (_.toString)

  val args: Parser[Seq[String]] = (arg.? ^^ (_.toList)) | ((arg ~ ("," ~> arg).*) ^^ {
    case a ~ as => a :: as
  })

  val attr: Parser[Attr] = ("." ~> ident) ~ ("(" ~> number <~ ")") ^^ {
    case name ~ num => Attr(name, num)
  }

  val property: Parser[Property] = (ident <~ ".") ~ ident ^^ {
    case target ~ name => Property(target, name)
  }

  val obj: Parser[Object] = ident ^^ Object.apply

  val funCall: Parser[FunCall] = (ident <~ ".") ~ ident ~ attr.* ~ ("(" ~> args <~ ")") ^^ {
    case target ~ fun ~ attributes ~ arguments =>
      FunCall(target, fun, arguments, attributes)
  }

  val accounts: Parser[Accounts.type] = "accounts" ^^ (_ => Accounts)

  val create: Parser[Create] = "create" ~> attr.* ~ ("(" ~> "\"" ~> ident <~ "\"" <~ ")") ^^ {
    case attributes ~ name => Create(name, attributes)
  }

  val doNothing: Parser[DoNothing.type] = "\\s*".r ^^ (_ => DoNothing)

  val cmd: Parser[Cmd] = create | accounts | funCall | property | obj | doNothing

  def apply(line: String): Either[RunnerError, Cmd] = parseAll(cmd, line) match {
    case NoSuccess(msg, _) => Left(ParserError(msg))
    case Success(matched, _) => Right(matched)
  }
}
