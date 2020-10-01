package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.healthcheck.Healthcheck

object JsonRpcHealthcheck {
  type T[R] = Healthcheck[JsonRpcError, R]

  def apply[R](description: String, f: () ⇒ ServiceResponse[R]): T[R] = Healthcheck(description, f)
}
