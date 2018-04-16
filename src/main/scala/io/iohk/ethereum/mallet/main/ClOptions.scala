package io.iohk.ethereum.mallet.main

import java.nio.file.Paths

import akka.http.scaladsl.model.Uri
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.mallet.common.Constants

case class ClOptions(
    node: Uri = Uri./,
    dataDir: String = Paths.get(System.getProperty("user.home"), "." + Constants.AppName).toString,
    command: Option[String] = None,
    account: Option[Address] = None,
    password: Option[String] = None
)
