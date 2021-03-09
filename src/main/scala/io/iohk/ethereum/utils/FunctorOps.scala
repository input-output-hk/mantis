package io.iohk.ethereum.utils

import cats.Functor

import scala.language.implicitConversions

class FunctorOps[A, F[_]: Functor](f: F[A]) {
  def tap[B](cb: A => Unit): F[A] =
    Functor[F].map(f) { a =>
      cb(a)
      a
    }
}
object FunctorOps {
  implicit def functorToFunctorOps[A, F[_]: Functor](f: F[A]): FunctorOps[A, F] = new FunctorOps(f)
}
