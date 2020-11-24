package io.iohk.ethereum.crypto

import io.iohk.ethereum.security.SecureRandomBuilder

/**
  * A simple tool to generate and ECDSA key pair. The key pair will be printed in the format:
  *   priv-key-hex (32 bytes)
  *   pub-key-hex (64 bytes)
  *
  * Run:
  *   ./eckeygen > mantis-datadir/node.key
  *
  * to generate the private key for the node. Note that only the private key will be read upon Mantis boot,
  * and the second line is equivalent to node ID.
  * The tool can also be used to generate keys for an Ethereum account.
  */
object EcKeyGen extends App with SecureRandomBuilder {
  val (prv, pub) = newRandomKeyPairAsStrings(secureRandom)
  //scalastyle:off
  println(prv + "\n" + pub)
}
