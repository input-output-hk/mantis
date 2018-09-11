package io.iohk.ethereum.utils

import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.PeerId

// Gathers together event-related stuff, so that you can easily import them all at once
package object events {
  final type EventDSL = io.riemann.riemann.client.EventDSL

  implicit class RichEventDSL(val event: EventDSL) extends AnyVal {
    def attribute(name: String, value: BigInt): EventDSL = event.attribute(name, value.toString())

    def attribute(name: String, value: Int): EventDSL = event.attribute(name, value.toString)

    def attribute(name: String, value: Long): EventDSL = event.attribute(name, value.toString)

    def timeTakenMs(ms: Long): EventDSL = attribute(EventAttr.TimeTakenMs, ms)

    def count(count: Int): EventDSL = attribute(EventAttr.Count, count)

    def peerId(peerId: PeerId): EventDSL = event.attribute(EventAttr.PeerId, peerId.value)

    def header(header: BlockHeader): EventDSL =
      event
        .attribute("header", header.number)
        .attribute("headerHex", "0x" + header.number.toString(16))
        .attribute("headerHash", "0x" + header.hashAsHexString)

    def block(block: Block): EventDSL =
      event
        .attribute("block", block.header.number)
        .attribute("blockHex", "0x" + block.header.number.toString(16))
        .attribute("blockHash", "0x" + block.header.hashAsHexString)

    def block(prefix: String, header: BlockHeader): EventDSL =
      event
        .attribute(s"${prefix}Block", header.number)
        .attribute(s"${prefix}BlockHex", "0x" + header.number.toString(16))
        .attribute(s"${prefix}BlockHash", "0x" + header.hashAsHexString)

    def block(prefix: String, block: Block): EventDSL = this.block(prefix, block.header)
  }

  object EventState {
    final val Critical = "critical"
    final val Error = "err"
    final val OK = "ok"
    final val Warning = "warning"
  }

  object EventTag {
    final val BlockForge = "blockForge"
    final val BlockImport = "blockImport"
    final val Exception = "exception"
    final val Metric = "metric"
    final val Finish = "finish"
    final val Start = "start"
    final val Unhandled = "unhandled"
  }

  object EventAttr {
    final val Count = "count"
    final val Error = "error"
    final val File = "file"
    final val Id = "id"
    final val PeerId = "peerId"
    final val Resource = "resource"
    final val Status = "status"
    final val ThreadId = "threadId"
    final val ThreadName = "threadName"
    final val ThreadPriority = "threadPriority"
    final val TimeTakenMs = "timeTakenMs"
    final val TimeUnit = "timeUnit"
    final val Type = "type"
    final val Unit = "unit"
  }
}
