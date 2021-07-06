package io.iohk.ethereum.extvm

import scala.io.Source

object ApiVersionProvider {
  private val ResourcePath = "extvm/VERSION"

  lazy val version: String = {
    val source = Source.fromResource(ResourcePath)
    try source.getLines().next()
    finally source.close()
  }
}
