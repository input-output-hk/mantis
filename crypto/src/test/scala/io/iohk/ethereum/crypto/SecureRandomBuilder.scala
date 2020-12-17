package io.iohk.ethereum.crypto

import java.security.SecureRandom

private[crypto] trait SecureRandomBuilder {
  lazy val secureRandom: SecureRandom = new SecureRandom()
}
