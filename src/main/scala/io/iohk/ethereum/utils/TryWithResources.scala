package io.iohk.ethereum.utils

import scala.util.control.NonFatal

object TryWithResources {

  // try-with-resources, source: https://github.com/dkomanov/stuff/blob/master/src/com/komanov/io/package.scala
  def withResources[R <: AutoCloseable, T](r: => R)(f: R => T): T = {
    val resource: R = r
    require(resource != null, "resource is null")
    var exception: Throwable = null
    try f(resource)
    catch {
      case NonFatal(e) =>
        exception = e
        throw e
    } finally closeAndAddSuppressed(exception, resource)
  }

  private def closeAndAddSuppressed(e: Throwable, resource: AutoCloseable): Unit =
    if (e != null) {
      try resource.close()
      catch {
        case NonFatal(suppressed) =>
          e.addSuppressed(suppressed)
      }
    } else {
      resource.close()
    }
}
