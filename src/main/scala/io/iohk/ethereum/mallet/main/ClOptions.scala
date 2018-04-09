package io.iohk.ethereum.mallet.main

import java.nio.file.Paths

import io.iohk.ethereum.mallet.common.Constants

case class ClOptions(
    node: String = "",
    dataDir: String = Paths.get(System.getProperty("user.home"), "." + Constants.AppName).toString,
    command: Option[String] = None,
    account: Option[String] = None,
    password: Option[String] = None
)
