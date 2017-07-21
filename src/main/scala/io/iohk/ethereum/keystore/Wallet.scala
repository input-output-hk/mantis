package io.iohk.ethereum.keystore


import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import org.spongycastle.crypto.AsymmetricCipherKeyPair

case class Wallet(address: Address, prvKey: ByteString) {
  lazy val keyPair: AsymmetricCipherKeyPair = keyPairFromPrvKey(prvKey.toArray)

  def signTx(tx: Transaction, chainId: Option[Byte]): SignedTransaction =
    SignedTransaction.sign(tx, keyPair, chainId)
}
