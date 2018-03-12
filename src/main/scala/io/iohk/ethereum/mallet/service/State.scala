package io.iohk.ethereum.mallet.service

import java.time.Instant

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.keystore.KeyStore

class State(
  val passwordReader: PasswordReader,
  val rpcClient: RpcClient,
  val keyStore: KeyStore,
  val selectedAccount: Option[Address],
  val unlockedKey: Option[ByteString],
  val keyLastTouched: Instant
) {

  def copy(
    passwordReader: PasswordReader = passwordReader,
    rpcClient: RpcClient = rpcClient,
    keyStore: KeyStore = keyStore,
    selectedAccount: Option[Address] = selectedAccount,
    unlockedKey: Option[ByteString] = unlockedKey,
    keyLastTouched: Instant = keyLastTouched
  ): State =
    new State(passwordReader, rpcClient, keyStore, selectedAccount, unlockedKey, keyLastTouched)


  def selectAccount(address: Address): State =
    copy(selectedAccount = Some(address))
}
