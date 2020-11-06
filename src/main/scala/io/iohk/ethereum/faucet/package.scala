package io.iohk.ethereum

package object faucet {
  sealed trait FaucetStatus
  sealed abstract trait FaucetWalletStatus extends FaucetStatus
  object FaucetStatus {
    case object FaucetUnavailable extends FaucetStatus
    case object WalletDoesNotExist extends FaucetWalletStatus
    //case object WalletIsLocked extends FaucetWalletStatus //TODO: review this state
    case object WalletIsUnlocked extends FaucetWalletStatus
    case object WalletNotResponds extends FaucetWalletStatus
  }
}
