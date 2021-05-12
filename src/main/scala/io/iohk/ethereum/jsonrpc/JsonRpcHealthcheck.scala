package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.healthcheck.HealthcheckResult
import monix.eval.Task

final case class JsonRpcHealthcheck[Response](
    description: String,
    healthCheck: Either[String, Response],
    info: Option[String] = None
) {

  def toResult: HealthcheckResult = {
    healthCheck
      .fold(
        HealthcheckResult.error(description, _),
        result => HealthcheckResult.ok(description, info)
      )
  }

  def withPredicate(message: String)(predicate: Response => Boolean): JsonRpcHealthcheck[Response] =
    copy(healthCheck = healthCheck.filterOrElse(predicate, message))

  def collect[T](message: String)(collectFn: PartialFunction[Response, T]): JsonRpcHealthcheck[T] =
    copy(
      description = description,
      healthCheck = healthCheck.flatMap(collectFn.lift(_).toRight(message))
    )

  def withInfo(getInfo: Response => String): JsonRpcHealthcheck[Response] =
    copy(info = healthCheck.toOption.map(getInfo))
}

object JsonRpcHealthcheck {

  def fromServiceResponse[Response](name: String, f: ServiceResponse[Response]): Task[JsonRpcHealthcheck[Response]] =
    f.map(result =>
      JsonRpcHealthcheck(
        name,
        result.left.map[String](_.message)
      )
    ).onErrorHandle(t => JsonRpcHealthcheck(name, Left(t.getMessage())))

  def fromTask[Response](name: String, f: Task[Response]): Task[JsonRpcHealthcheck[Response]] =
    f.map(result =>
      JsonRpcHealthcheck(
        name,
        Right(result)
      )
    ).onErrorHandle(t => JsonRpcHealthcheck(name, Left(t.getMessage())))

}
