package io.iohk.ethereum.utils

object LoggingUtils {

  def getClassName(cls: Class[_]): String = cls.getName.split("\\.").last

  def getClassName(o: Object): String = getClassName(o.getClass)

}
