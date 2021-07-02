package io.iohk.ethereum.ledger

import akka.util.ByteString

import io.iohk.ethereum.domain.TxLogEntry
import io.iohk.ethereum.vm.ProgramError

case class TxResult(
    worldState: InMemoryWorldStateProxy,
    gasUsed: BigInt,
    logs: Seq[TxLogEntry],
    vmReturnData: ByteString,
    vmError: Option[ProgramError]
)
