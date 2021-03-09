package io.iohk.ethereum.crypto

import io.iohk.ethereum.security.SecureRandomBuilder

/** A simple tool to generate ECDSA key pairs. Takes an optional positional argument [n] - number of key pairs
  * to generate (default is 1).
  * The key pairs will be printed in the format:
  *   priv-key-hex (32 bytes)
  *   pub-key-hex (64 bytes)
  *
  * Run:
  *   ./eckeygen [n] > mantis-datadir/node.key
  *
  * to generate the private key for the node. Note that only the private key will be read upon Mantis boot,
  * and the second line is equivalent to node ID.
  * The tool can also be used to generate keys for an Ethereum account.
  */
object EcKeyGen extends App with SecureRandomBuilder {
  val numOfKeys: Int = args.headOption.map(_.toInt).getOrElse(1)

  val keyPairs: IndexedSeq[(String, String)] = for (_ <- 1 to numOfKeys) yield newRandomKeyPairAsStrings(secureRandom)

  //scalastyle:off
  println(keyPairs.map { case (prv, pub) => s"$prv\n$pub\n" }.mkString("\n"))
}
