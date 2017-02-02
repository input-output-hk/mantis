package io.iohk.ethereum.utils

import scala.util.{Either, Left, Right}

// To be removed after upgrading Scala to v2.12
object EitherExtensions {

  implicit class RightBiasedEither[A, B](val either: Either[A, B]) extends AnyVal {
    def map[Y](f: B => Y): Either[A, Y] = either match {
      case Left(a) => Left(a)
      case Right(b) => Right(f(b))
    }

    def flatMap[AA >: A, Y](f: B => Either[AA, Y]): Either[AA, Y] = either match {
      case Left(a) => Left(a)
      case Right(b) => f(b)
    }

    def get: B = either.right.get

    def getOrElse[BB >: B](or: => BB): BB = either.right.getOrElse(or)

    def valueOr[BB >: B](f: A => BB): BB = either match {
      case Left(a)  => f(a)
      case Right(b) => b
    }
  }

  implicit class EitherIdOps[A](val obj: A) extends AnyVal {
    def asLeft[B]: Either[A, B] = Left(obj)

    def asRight[B]: Either[B, A] = Right(obj)
  }
}
