package io.iohk.ethereum.network

import cats._
import cats.implicits._

case class PeerStat(
    responsesReceived: Int,
    requestsReceived: Int,
    firstSeenTimeMillis: Option[Long],
    lastSeenTimeMillis: Option[Long]
)
object PeerStat {
  val empty: PeerStat = PeerStat(0, 0, None, None)

  private def mergeOpt[A, B](x: A, y: A)(f: A => Option[B])(g: (B, B) => B): Option[B] = {
    val (mx, my) = (f(x), f(y))
    (mx, my).mapN(g).orElse(mx).orElse(my)
  }

  implicit val monoid: Monoid[PeerStat] =
    Monoid.instance(
      empty,
      (a, b) =>
        PeerStat(
          responsesReceived = a.responsesReceived + b.responsesReceived,
          requestsReceived = a.requestsReceived + b.requestsReceived,
          firstSeenTimeMillis = mergeOpt(a, b)(_.firstSeenTimeMillis)(math.min),
          lastSeenTimeMillis = mergeOpt(a, b)(_.lastSeenTimeMillis)(math.max)
        )
    )
}
