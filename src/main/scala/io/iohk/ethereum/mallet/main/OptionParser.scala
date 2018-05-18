package io.iohk.ethereum.mallet.main

import akka.http.scaladsl.model.Uri
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.mallet.common.{Constants, StringUtil}

/** Command line options and positional args parser */
object OptionParser {

  private implicit val uriRead: scopt.Read[Uri] =
    scopt.Read.reads(Uri.apply)

  private implicit val addrRead: scopt.Read[Address] =
    scopt.Read.reads { str =>
      val bytes = StringUtil.hexToBytes(str)
      require(bytes.nonEmpty && bytes.length <= Address.Length, "invalid address length")
      Address(bytes)
    }

  def apply(args: Seq[String]): Option[ClOptions] = {
    val parser = new scopt.OptionParser[ClOptions](Constants.AppName) {
      head(Constants.AppName, "0.1") //TODO: proper versioning

      opt[String]('d', "data-dir")
        .action((d, o) => o.copy(dataDir = d))
        .text(s"customise the directory where keys et. al. are kept (default: ~/.${Constants.AppName})")

      opt[String]('c', "command")
        .action((c, o) => o.copy(command = Some(c)))
        .text("runs mallet in non-interactive mode - provided command will be executed and the app will exit. " +
          "Exit code 0 indicates success, failure otherwise")

      opt[Address]('a', "account")
        .action((a, o) => o.copy(account = Some(a)))
        .text("an alternative to 'selectAccount' command")

      opt[String]('p', "password")
        .action((p, o) => o.copy(password = Some(p)))
        .text("when non-interactive is used, the password for accessing account may be provided this way " +
          "(make sure no one's looking!)")

      arg[Uri]("<node>")
        .action((n, o) => o.copy(node = n))
        .validate { uri =>
          if (!Set("http", "https").contains(uri.scheme))
            failure("node URI scheme must be explicit and either 'http' or 'https'")
          else
            success
        }
        .text("URI of the node that mallet connects to (HTTP or HTTPS)")

      help("help")
        .text("prints this usage text")
    }

    parser.parse(args, ClOptions())
  }
}
