package io.iohk.ethereum.eventbus.event

import io.iohk.ethereum.domain.Block

case class NewHead(blockRemoved: Seq[Block], blocksAdded: Seq[Block])
