package io.iohk.ethereum.utils

object ValidationUtils {

  /** This function combines multiple validations on object.
    *
    * @param obj object to return if all validations pass .
    * @param eithers list of required validations.
    * @return object if all validations pass, else non-empty set of errors.
    */
  def combineValidations[A, B](obj: B, eithers: Either[A, B]*): Either[Set[A], B] = {
    val errors = eithers.collect { case Left(e) => e }
    if (errors.isEmpty) Right(obj) else Left(errors.toSet)
  }
}
