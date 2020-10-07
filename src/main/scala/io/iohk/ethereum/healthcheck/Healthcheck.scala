package io.iohk.ethereum.healthcheck

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Represents a health check, runs it and interprets the outcome.
  * The outcome can be either a normal result, an application error, or
  * an (unexpected) exception.
  *
  * @param description An one-word description of the health check.
  * @param f           The function that runs the health check.
  * @param mapResultToError A function that interprets the result.
  * @param mapErrorToError  A function that interprets the application error.
  * @param mapExceptionToError A function that interprets the (unexpected) exception.
  * @tparam Error  The type of the application error.
  * @tparam Result The type of the actual value expected by normal termination of `f`.
  */
case class Healthcheck[Error, Result](
    description: String,
    f: () => Future[Either[Error, Result]],
    mapResultToError: Result => Option[String] = (_: Result) => None,
    mapErrorToError: Error => Option[String] = (error: Error) => Some(String.valueOf(error)),
    mapExceptionToError: Throwable => Option[String] = (t: Throwable) => Some(String.valueOf(t))
) {

  def apply()(implicit ec: ExecutionContext): Future[HealthcheckResult] = {
    f().transform {
      case Success(Left(error)) =>
        Success(HealthcheckResult(description, mapErrorToError(error)))
      case Success(Right(result)) =>
        Success(HealthcheckResult(description, mapResultToError(result)))
      case Failure(t) =>
        Success(HealthcheckResult(description, mapExceptionToError(t)))
    }
  }
}
