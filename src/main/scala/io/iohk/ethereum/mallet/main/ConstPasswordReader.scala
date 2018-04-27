package io.iohk.ethereum.mallet.main

import io.iohk.ethereum.mallet.service.PasswordReader

/**
  * This implementation of [[io.iohk.ethereum.mallet.service.PasswordReader PasswordReader]] is used in
  * non-interactive mode when the password is provided as a command line option
  */
class ConstPasswordReader(password: String) extends PasswordReader {
  def readPassword(): Option[String] =
    Some(password)

  def readPasswordTwice(): Option[String] =
    Some(password)
}
