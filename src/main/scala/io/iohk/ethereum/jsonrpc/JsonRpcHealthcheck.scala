package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.healthcheck.HealthcheckResult
import monix.eval.Task

final case class JsonRpcHealthcheck[Response](
    description: String,
    healthCheckTask: Task[Either[String, Response]],
    getInfo: Option[Response => String] = None
) {

  def toTask: Task[HealthcheckResult] = {
    healthCheckTask
      .map {
        case Left(errorMsg) =>
          HealthcheckResult.error(description, errorMsg)
        case Right(result) =>
          HealthcheckResult.ok(description, getInfo.map(get => get(result)))
      }
      .onErrorHandle(t => HealthcheckResult.error(description, t.getMessage()))
  }

  def withPredicate(message: String)(predicate: Response => Boolean): JsonRpcHealthcheck[Response] =
    copy(healthCheckTask = healthCheckTask.map(_.filterOrElse(predicate, message)))

  def collect[T](message: String)(collectFn: PartialFunction[Response, T]): JsonRpcHealthcheck[T] =
    JsonRpcHealthcheck(
      description = description,
      healthCheckTask = healthCheckTask.map(_.flatMap(collectFn.lift(_).toRight(message)))
    )

  def withInfo(getInfo: Response => String): JsonRpcHealthcheck[Response] =
    copy(getInfo = Some(getInfo))
}

object JsonRpcHealthcheck {

  def fromServiceResponse[Response](name: String, f: ServiceResponse[Response]): JsonRpcHealthcheck[Response] =
    JsonRpcHealthcheck(
      name,
      f.map(_.left.map[String](_.message))
    )

  def fromTask[Response](name: String, f: Task[Response]): JsonRpcHealthcheck[Response] =
    JsonRpcHealthcheck(
      name,
      f.map(Right.apply)
    )

}
