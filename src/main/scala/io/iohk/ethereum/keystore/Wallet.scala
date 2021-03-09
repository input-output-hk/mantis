package io.iohk.ethereum.keystore

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.domain.SignedTransactionWithSender
import io.iohk.ethereum.domain.Transaction
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

case class Wallet(address: Address, prvKey: ByteString) {
  lazy val keyPair: AsymmetricCipherKeyPair = keyPairFromPrvKey(prvKey.toArray)

  def signTx(tx: Transaction, chainId: Option[Byte]): SignedTransactionWithSender =
    SignedTransaction.sign(tx, keyPair, chainId)
}
