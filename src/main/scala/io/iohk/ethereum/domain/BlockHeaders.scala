package io.iohk.ethereum.domain

object BlockHeaders {
  def lastNumber(headers: BlockHeaders): Option[BigInt] = headers.lastOption.map(_.number)

  def areChain(headers: BlockHeaders): Boolean = {
    if (headers.length > 1)
      headers.zip(headers.tail).forall {
        case (parent, child) =>
          parent.hash == child.parentHash && parent.number + 1 == child.number
      } else
      headers.nonEmpty
  }
}
