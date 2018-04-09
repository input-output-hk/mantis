package io.iohk.ethereum.mallet.main

import io.iohk.ethereum.mallet.service.PasswordReader

class ConstPasswordReader(password: String) extends PasswordReader {
  def readPassword(): Option[String] =
    Some(password)

  def readPasswordTwice(): Option[String] =
    Some(password)
}
