package io.iohk.ethereum.keystore


import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}
import org.spongycastle.crypto.AsymmetricCipherKeyPair

case class Wallet(address: Address, keyPair: AsymmetricCipherKeyPair) {
  def signTx(tx: Transaction): SignedTransaction =
    SignedTransaction.sign(tx, keyPair, 1)
}
