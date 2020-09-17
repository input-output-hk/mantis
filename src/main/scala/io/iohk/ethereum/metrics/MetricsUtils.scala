package io.iohk.ethereum.metrics

object MetricsUtils {

  def mkNameWithPrefix(prefix: String)(name: String): String = {
    val metricsPrefix = prefix + "."
    if (name.startsWith(metricsPrefix)) name else metricsPrefix + name
  } 

}
