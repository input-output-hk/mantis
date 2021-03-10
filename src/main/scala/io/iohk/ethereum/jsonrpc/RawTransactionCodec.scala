package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.network.p2p.messages.PV60.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.rlp

object RawTransactionCodec {

  def asRawTransaction(e: SignedTransaction): ByteString =
    ByteString(rlp.encode(e.toRLPEncodable))
}
