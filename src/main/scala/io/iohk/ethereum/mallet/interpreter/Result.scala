package io.iohk.ethereum.mallet.interpreter

import io.iohk.ethereum.mallet.service.State

object Result {
  def success(msg: String, state: State): Result =
    Result(msg, state, error = false)

  def error(msg: String, state: State): Result =
    Result(msg, state, error = true)
}

case class Result(msg: String, state: State, error: Boolean)
