package io.iohk.ethereum

import io.iohk.ethereum.utils.{RLPSpeedSuite, RLPSuite}
import org.scalatest.Suites

class EtcClientTestSuite extends Suites(
  new RLPSpeedSuite,
  new RLPSuite
)

