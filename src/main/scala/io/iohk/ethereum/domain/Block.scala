package io.iohk.ethereum.domain

import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody


/**
  * This class represent a block as a header and a body which are returned in two different messages
  *
  * @param header Block header
  * @param body Block body
  */
case class Block (header: BlockHeader, body: BlockBody) {
  override def toString: String = {
    s"""BlockHeader {
       | header: $header
       | body: $body
     """.stripMargin
  }
}
