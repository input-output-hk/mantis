package io.iohk.ethereum.mallet.service

trait PasswordReader {
  def readPassword(): Option[String]
  def readPasswordTwice(): Option[String]
}
