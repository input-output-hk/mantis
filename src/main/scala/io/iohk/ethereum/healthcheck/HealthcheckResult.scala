package io.iohk.ethereum.healthcheck

final case class HealthcheckResult private(description: String, status: String, error: Option[String]) {
  assert(
    status == HealthcheckStatus.OK && error.isEmpty || status == HealthcheckStatus.ERROR && error.isDefined
  )

  def isOK: Boolean = status == HealthcheckStatus.OK
}

object HealthcheckResult {
  def apply(description: String, error: Option[String]): HealthcheckResult =
    new HealthcheckResult(
      description = description,
      status = error.fold(HealthcheckStatus.OK)(_ ⇒ HealthcheckStatus.ERROR),
      error = error
    )
}
