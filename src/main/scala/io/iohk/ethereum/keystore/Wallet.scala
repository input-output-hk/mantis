package io.iohk.ethereum.keystore


import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, SignedTransaction, Transaction}

case class Wallet(address: Address, privKey: ByteString) {
  def signTx(tx: Transaction): SignedTransaction = ???
}
