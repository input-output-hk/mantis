package io.iohk.ethereum.mallet.main

/** Command line options and positional args parser */
object OptionParser {

  // TODO: add help strings and validations

  def apply(args: Seq[String]): Option[ClOptions] = {
    val parser = new scopt.OptionParser[ClOptions]("scopt") {
      opt[String]('d', "data-dir").action((d, o) => o.copy(dataDir = d))

      opt[String]('c', "command").action((c, o) => o.copy(command = Some(c)))

      opt[String]('a', "account").action((a, o) => o.copy(account = Some(a)))

      opt[String]('p', "password").action((p, o) => o.copy(password = Some(p)))

      arg[String]("<node>").action((n, o) => o.copy(node = n))
    }

    parser.parse(args, ClOptions())
  }
}
