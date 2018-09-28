package io.iohk.ethereum.vm

object Fixtures {

  val blockchainConfig = BlockchainConfigForEvm(
    // block numbers are irrelevant
    frontierBlockNumber = 0,
    homesteadBlockNumber = 0,
    eip150BlockNumber = 0,
    eip160BlockNumber = 0,
    eip161BlockNumber = 0,
    danseBlockNumber = 0,
    maxCodeSize = None,
    accountStartNonce = 0
  )

}
