package io.iohk.ethereum.domain.appstate

import akka.util.ByteString

case class BlockInfo(hash: ByteString, number: BigInt)
