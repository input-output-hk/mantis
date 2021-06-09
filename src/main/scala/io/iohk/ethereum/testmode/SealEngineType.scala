package io.iohk.ethereum.testmode

sealed trait SealEngineType

object SealEngineType {
  // Do not check `nonce` and `mixhash` field in blockHeaders
  object NoProof extends SealEngineType
  // Do not check `nonce` and `mixhash` field in blockHeaders + Do not check mining reward (block + uncle headers)
  object NoReward extends SealEngineType
}
