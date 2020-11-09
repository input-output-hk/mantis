package io.iohk.ethereum.proof

import akka.util.ByteString

final case class Proof[V](
    value: Option[V],
    proofRelatedNodes: List[ByteString]
) // TODO Bytes => ByteString is it right?
