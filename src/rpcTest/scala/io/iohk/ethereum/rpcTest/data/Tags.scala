package io.iohk.ethereum.rpcTest.data
import org.scalatest.Tag

object Tags {
  object MainNet extends Tag("MainNet")
  object PrivNet extends Tag("PrivNet")
  object PrivNetNoMining extends Tag("PrivNetNoMining")
  object Morden extends Tag("Morden")
  object Ropsten extends Tag("Ropsten")
  object Standalone extends Tag("Standalone")
  object Net extends Tag("Net")
  object Eth extends Tag("Eth")
  object Personal extends Tag("Personal")
}
