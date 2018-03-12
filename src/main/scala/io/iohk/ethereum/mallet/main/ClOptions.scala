package io.iohk.ethereum.mallet.main

import java.io.File
import java.nio.file.Paths

import io.iohk.ethereum.mallet.common.Constants

case class ClOptions(
    node: String = "",
    dataDir: File = Paths.get(System.getProperty("user.home"), "." + Constants.AppName).toFile,
    command: Option[String] = None
)
