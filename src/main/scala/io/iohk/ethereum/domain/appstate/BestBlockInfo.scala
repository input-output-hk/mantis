package io.iohk.ethereum.domain.appstate

import akka.util.ByteString

case class BestBlockInfo(hash: ByteString, number: BigInt)
