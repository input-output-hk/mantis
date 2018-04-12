package io.iohk.ethereum.mallet.common

import java.io.{PrintWriter, StringWriter}
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

object Util {

  /**
    * Finds sealed descendant objects of a base class/trait via reflection
    * @tparam T base type
    * @return a set of objects of type T
    */
  def sealedDescendants[T: TypeTag]: Set[T] = {
    val symbol = typeOf[T].typeSymbol
    val internal = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol]
    val descendants = internal.sealedDescendants.map(_.asInstanceOf[Symbol]) - symbol
    descendants.map { d =>
      val module = d.owner.typeSignature.member(d.name.toTermName)
      currentMirror.reflectModule(module.asModule).instance.asInstanceOf[T]
    }
  }

  def exceptionToString(ex: Throwable): String = {
    val sw = new StringWriter()
    sw.append(ex.getMessage + "\n")
    ex.printStackTrace(new PrintWriter(sw))
    sw.toString
  }

  implicit class OptionOps[T](opt: Option[T]) {
    def toEither[U](left: U): Either[U, T] =
      opt.map(Right(_)).getOrElse(Left(left))
  }
}
