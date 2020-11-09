package io.iohk.ethereum

package object faucet {
  sealed trait FaucetStatus
  object FaucetStatus {
    case object FaucetUnavailable extends FaucetStatus
    case object WalletDoesNotExist extends FaucetStatus
    case object WalletAvailable extends FaucetStatus
    case object WalletNotResponds extends FaucetStatus
  }
}
