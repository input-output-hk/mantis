package io.iohk.ethereum.utils
import org.scalacheck.Gen

object GenOps {
  implicit class GenOps[T](gen: Gen[T]) {
    def pickValue: T =
      Iterator
        .continually(gen)
        .map(_.sample)
        .collectFirst { case Some(value) =>
          value
        }
        .get
  }
}
