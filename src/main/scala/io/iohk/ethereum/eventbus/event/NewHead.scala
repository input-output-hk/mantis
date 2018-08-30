package io.iohk.ethereum.eventbus.event

import io.iohk.ethereum.domain.Block

case class NewHead(block: Block)
