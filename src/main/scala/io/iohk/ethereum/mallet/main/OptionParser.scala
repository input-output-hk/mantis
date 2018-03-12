package io.iohk.ethereum.mallet.main

import java.io.File


object OptionParser {

  def apply(args: Seq[String]): Option[ClOptions] = {
    val parser = new scopt.OptionParser[ClOptions]("scopt") {
      opt[File]('d', "data-dir").action((d, o) => o.copy(dataDir = d))

      opt[String]('c', "command").action((c, o) => o.copy(command = Some(c)))

      arg[String]("<node>").action((n, o) => o.copy(node = n))
    }

    parser.parse(args, ClOptions())
  }
}
