package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.rlp

object RawTransactionCodec {

  def rawTransactionFromBlock(blockTxs: Seq[SignedTransaction], index: Int): Option[ByteString] =
    blockTxs.lift(index).map(asRawTransaction)

  def asRawTransaction(e: SignedTransaction): ByteString =
    ByteString(rlp.encode(e.toRLPEncodable))
}
