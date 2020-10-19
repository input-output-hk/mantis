package io.iohk.ethereum.mallet.common

sealed trait Err {
  def msg: String
}

case class ParserError(msg: String) extends Err

case class RpcClientError(msg: String) extends Err
