package io.iohk.ethereum

import java.security.SecureRandom

import io.iohk.ethereum.utils.Config

trait SecureRandomProvider {
  val secureRandom: SecureRandom = SecureRandom.getInstance(Config.secureRandomAlgo)
}

object SecureRandomProvider extends SecureRandomProvider
