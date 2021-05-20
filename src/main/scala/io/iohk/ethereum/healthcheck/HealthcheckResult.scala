package io.iohk.ethereum.healthcheck

final case class HealthcheckResult private (
    name: String,
    status: String,
    info: Option[String]
) {

  def isOK: Boolean = status == HealthcheckStatus.OK
}

object HealthcheckResult {

  def ok(name: String, info: Option[String] = None): HealthcheckResult =
    new HealthcheckResult(
      name = name,
      status = HealthcheckStatus.OK,
      info = info
    )

  def error(name: String, error: String): HealthcheckResult =
    new HealthcheckResult(
      name = name,
      status = HealthcheckStatus.ERROR,
      info = Some(error)
    )
}
