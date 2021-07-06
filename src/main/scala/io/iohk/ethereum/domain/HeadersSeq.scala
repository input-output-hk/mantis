package io.iohk.ethereum.domain

object HeadersSeq {
  def lastNumber(headers: HeadersSeq): Option[BigInt] = headers.lastOption.map(_.number)

  def areChain(headers: HeadersSeq): Boolean =
    if (headers.length > 1)
      headers.zip(headers.tail).forall { case (parent, child) =>
        parent.hash == child.parentHash && parent.number + 1 == child.number
      }
    else
      headers.nonEmpty
}
