package io.iohk.ethereum.healthcheck

final case class HealthcheckResponse(checks: List[HealthcheckResult]) {
  lazy val isOK: Boolean = checks.forall(_.isOK)
}
