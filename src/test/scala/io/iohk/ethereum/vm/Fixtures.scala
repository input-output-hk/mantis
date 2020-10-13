package io.iohk.ethereum.vm

object Fixtures {

  val ConstantinopleBlockNumber = 200
  val PetersburgBlockNumber = 400
  val PhoenixBlockNumber = 600
  val IstanbulBlockNumber = 600

  val blockchainConfig = BlockchainConfigForEvm(
    // block numbers are irrelevant
    frontierBlockNumber = 0,
    homesteadBlockNumber = 0,
    eip150BlockNumber = 0,
    eip160BlockNumber = 0,
    eip161BlockNumber = 0,
    byzantiumBlockNumber = 0,
    constantinopleBlockNumber = ConstantinopleBlockNumber,
    istanbulBlockNumber = IstanbulBlockNumber,
    maxCodeSize = None,
    accountStartNonce = 0,
    atlantisBlockNumber = 0,
    aghartaBlockNumber = 0,
    petersburgBlockNumber = PetersburgBlockNumber,
    phoenixBlockNumber = PhoenixBlockNumber,
    chainId = 0x3d.toByte
  )

}
