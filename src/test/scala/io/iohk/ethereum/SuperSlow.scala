package io.iohk.ethereum

import com.typesafe.config.ConfigFactory

trait SuperSlow {
  lazy private val skip = ConfigFactory.load().getBoolean("skip-super-slow-tests")

  /**
    * Some assertions may be prohibitively slow and shouldn't run on every CI run. Use this method when that's the case.
    *
    * @param f slow tests
    */
  def superSlow[T](f: => T): Option[T] =
    if (!skip) Some(f) else None
}
