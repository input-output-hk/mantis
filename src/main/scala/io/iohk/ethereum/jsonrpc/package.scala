package io.iohk.ethereum

import monix.eval.Task

package object jsonrpc {
  type ServiceResponse[T] = Task[Either[JsonRpcError, T]]
}
