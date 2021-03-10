package io.iohk.ethereum.ets.blockchain

import akka.util.ByteString
import io.iohk.ethereum.consensus.Protocol
import io.iohk.ethereum.consensus.ethash.validators.ValidatorsExecutor
import io.iohk.ethereum.domain.{Address, UInt256}
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig, MonetaryPolicyConfig}
import org.bouncycastle.util.encoders.Hex

// scalastyle:off magic.number
object BlockchainTestConfig {

  val BaseBlockchainConfig = BlockchainConfig(
    frontierBlockNumber = Long.MaxValue,
    homesteadBlockNumber = Long.MaxValue,
    //Enabling maxGasLimit in all Configs and all blocks
    eip106BlockNumber = 0,
    eip150BlockNumber = Long.MaxValue,
    eip155BlockNumber = Long.MaxValue,
    eip160BlockNumber = Long.MaxValue,
    eip161BlockNumber = Long.MaxValue,
    byzantiumBlockNumber = Long.MaxValue,
    constantinopleBlockNumber = Long.MaxValue,
    petersburgBlockNumber = Long.MaxValue,
    istanbulBlockNumber = Long.MaxValue,
    // unused
    maxCodeSize = None,
    difficultyBombPauseBlockNumber = 3000000,
    difficultyBombContinueBlockNumber = 5000000,
    difficultyBombRemovalBlockNumber = 5900000,
    chainId = 0x1.toByte,
    networkId = 1,
    protocolVersion = 63,
    customGenesisFileOpt = Some("test-genesis.json"),
    monetaryPolicyConfig =
      MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"), BigInt("3000000000000000000")),
    daoForkConfig = None,
    accountStartNonce = UInt256.Zero,
    bootstrapNodes = Set(),
    // TODO: only place where this was supposed to be used but now it seems it's not, remove? Issue: EC-312
    gasTieBreaker = false,
    ethCompatibleStorage = true,
    atlantisBlockNumber = Long.MaxValue,
    aghartaBlockNumber = Long.MaxValue,
    phoenixBlockNumber = Long.MaxValue,
    ecip1098BlockNumber = Long.MaxValue,
    treasuryAddress = Address(0),
    ecip1097BlockNumber = Long.MaxValue,
    ecip1099BlockNumber = Long.MaxValue
  )

  val FrontierConfig = BaseBlockchainConfig.copy(
    frontierBlockNumber = 0
  )
  val HomesteadConfig = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = 0
  )
  val Eip150Config = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = -1,
    eip150BlockNumber = 0
  )
  val FrontierToHomesteadAt5 = BaseBlockchainConfig.copy(
    frontierBlockNumber = 0,
    homesteadBlockNumber = 5
  )
  val HomesteadToEIP150At5 = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = 0,
    eip150BlockNumber = 5
  )
  val HomesteadToDaoAt5 = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = 0,
    daoForkConfig = Some(
      new DaoForkConfig {
        override val forkBlockNumber: BigInt = 5
        override val forkBlockHash =
          ByteString(Hex.decode("f6d7ef1087b5fd94eada533cf8a563f78c3944a2f8ae850e80935d20dc3b7315"))
        override val blockExtraData = Some(ByteString(Hex.decode("64616f2d686172642d666f726b")))
        override val range = 10
        override val refundContract = Some(Address("bf4ed7b27f1d666546e30d74d50d173d20bca754"))
        override val drainList = Seq(
          Address("d4fe7bc31cedb7bfb8a345f31e668033056b2728"),
          Address("b3fb0e5aba0e20e5c49d252dfd30e102b171a425"),
          Address("2c19c7f9ae8b751e37aeb2d93a699722395ae18f"),
          Address("ecd135fa4f61a655311e86238c92adcd779555d2"),
          Address("1975bd06d486162d5dc297798dfc41edd5d160a7"),
          Address("a3acf3a1e16b1d7c315e23510fdd7847b48234f6"),
          Address("319f70bab6845585f412ec7724b744fec6095c85"),
          Address("06706dd3f2c9abf0a21ddcc6941d9b86f0596936"),
          Address("5c8536898fbb74fc7445814902fd08422eac56d0"),
          Address("6966ab0d485353095148a2155858910e0965b6f9"),
          Address("779543a0491a837ca36ce8c635d6154e3c4911a6"),
          Address("2a5ed960395e2a49b1c758cef4aa15213cfd874c"),
          Address("5c6e67ccd5849c0d29219c4f95f1a7a93b3f5dc5"),
          Address("9c50426be05db97f5d64fc54bf89eff947f0a321"),
          Address("200450f06520bdd6c527622a273333384d870efb"),
          Address("be8539bfe837b67d1282b2b1d61c3f723966f049"),
          Address("6b0c4d41ba9ab8d8cfb5d379c69a612f2ced8ecb"),
          Address("f1385fb24aad0cd7432824085e42aff90886fef5"),
          Address("d1ac8b1ef1b69ff51d1d401a476e7e612414f091"),
          Address("8163e7fb499e90f8544ea62bbf80d21cd26d9efd"),
          Address("51e0ddd9998364a2eb38588679f0d2c42653e4a6"),
          Address("627a0a960c079c21c34f7612d5d230e01b4ad4c7"),
          Address("f0b1aa0eb660754448a7937c022e30aa692fe0c5"),
          Address("24c4d950dfd4dd1902bbed3508144a54542bba94"),
          Address("9f27daea7aca0aa0446220b98d028715e3bc803d"),
          Address("a5dc5acd6a7968a4554d89d65e59b7fd3bff0f90"),
          Address("d9aef3a1e38a39c16b31d1ace71bca8ef58d315b"),
          Address("63ed5a272de2f6d968408b4acb9024f4cc208ebf"),
          Address("6f6704e5a10332af6672e50b3d9754dc460dfa4d"),
          Address("77ca7b50b6cd7e2f3fa008e24ab793fd56cb15f6"),
          Address("492ea3bb0f3315521c31f273e565b868fc090f17"),
          Address("0ff30d6de14a8224aa97b78aea5388d1c51c1f00"),
          Address("9ea779f907f0b315b364b0cfc39a0fde5b02a416"),
          Address("ceaeb481747ca6c540a000c1f3641f8cef161fa7"),
          Address("cc34673c6c40e791051898567a1222daf90be287"),
          Address("579a80d909f346fbfb1189493f521d7f48d52238"),
          Address("e308bd1ac5fda103967359b2712dd89deffb7973"),
          Address("4cb31628079fb14e4bc3cd5e30c2f7489b00960c"),
          Address("ac1ecab32727358dba8962a0f3b261731aad9723"),
          Address("4fd6ace747f06ece9c49699c7cabc62d02211f75"),
          Address("440c59b325d2997a134c2c7c60a8c61611212bad"),
          Address("4486a3d68fac6967006d7a517b889fd3f98c102b"),
          Address("9c15b54878ba618f494b38f0ae7443db6af648ba"),
          Address("27b137a85656544b1ccb5a0f2e561a5703c6a68f"),
          Address("21c7fdb9ed8d291d79ffd82eb2c4356ec0d81241"),
          Address("23b75c2f6791eef49c69684db4c6c1f93bf49a50"),
          Address("1ca6abd14d30affe533b24d7a21bff4c2d5e1f3b"),
          Address("b9637156d330c0d605a791f1c31ba5890582fe1c"),
          Address("6131c42fa982e56929107413a9d526fd99405560"),
          Address("1591fc0f688c81fbeb17f5426a162a7024d430c2"),
          Address("542a9515200d14b68e934e9830d91645a980dd7a"),
          Address("c4bbd073882dd2add2424cf47d35213405b01324"),
          Address("782495b7b3355efb2833d56ecb34dc22ad7dfcc4"),
          Address("58b95c9a9d5d26825e70a82b6adb139d3fd829eb"),
          Address("3ba4d81db016dc2890c81f3acec2454bff5aada5"),
          Address("b52042c8ca3f8aa246fa79c3feaa3d959347c0ab"),
          Address("e4ae1efdfc53b73893af49113d8694a057b9c0d1"),
          Address("3c02a7bc0391e86d91b7d144e61c2c01a25a79c5"),
          Address("0737a6b837f97f46ebade41b9bc3e1c509c85c53"),
          Address("97f43a37f595ab5dd318fb46e7a155eae057317a"),
          Address("52c5317c848ba20c7504cb2c8052abd1fde29d03"),
          Address("4863226780fe7c0356454236d3b1c8792785748d"),
          Address("5d2b2e6fcbe3b11d26b525e085ff818dae332479"),
          Address("5f9f3392e9f62f63b8eac0beb55541fc8627f42c"),
          Address("057b56736d32b86616a10f619859c6cd6f59092a"),
          Address("9aa008f65de0b923a2a4f02012ad034a5e2e2192"),
          Address("304a554a310c7e546dfe434669c62820b7d83490"),
          Address("914d1b8b43e92723e64fd0a06f5bdb8dd9b10c79"),
          Address("4deb0033bb26bc534b197e61d19e0733e5679784"),
          Address("07f5c1e1bc2c93e0402f23341973a0e043f7bf8a"),
          Address("35a051a0010aba705c9008d7a7eff6fb88f6ea7b"),
          Address("4fa802324e929786dbda3b8820dc7834e9134a2a"),
          Address("9da397b9e80755301a3b32173283a91c0ef6c87e"),
          Address("8d9edb3054ce5c5774a420ac37ebae0ac02343c6"),
          Address("0101f3be8ebb4bbd39a2e3b9a3639d4259832fd9"),
          Address("5dc28b15dffed94048d73806ce4b7a4612a1d48f"),
          Address("bcf899e6c7d9d5a215ab1e3444c86806fa854c76"),
          Address("12e626b0eebfe86a56d633b9864e389b45dcb260"),
          Address("a2f1ccba9395d7fcb155bba8bc92db9bafaeade7"),
          Address("ec8e57756626fdc07c63ad2eafbd28d08e7b0ca5"),
          Address("d164b088bd9108b60d0ca3751da4bceb207b0782"),
          Address("6231b6d0d5e77fe001c2a460bd9584fee60d409b"),
          Address("1cba23d343a983e9b5cfd19496b9a9701ada385f"),
          Address("a82f360a8d3455c5c41366975bde739c37bfeb8a"),
          Address("9fcd2deaff372a39cc679d5c5e4de7bafb0b1339"),
          Address("005f5cee7a43331d5a3d3eec71305925a62f34b6"),
          Address("0e0da70933f4c7849fc0d203f5d1d43b9ae4532d"),
          Address("d131637d5275fd1a68a3200f4ad25c71a2a9522e"),
          Address("bc07118b9ac290e4622f5e77a0853539789effbe"),
          Address("47e7aa56d6bdf3f36be34619660de61275420af8"),
          Address("acd87e28b0c9d1254e868b81cba4cc20d9a32225"),
          Address("adf80daec7ba8dcf15392f1ac611fff65d94f880"),
          Address("5524c55fb03cf21f549444ccbecb664d0acad706"),
          Address("40b803a9abce16f50f36a77ba41180eb90023925"),
          Address("fe24cdd8648121a43a7c86d289be4dd2951ed49f"),
          Address("17802f43a0137c506ba92291391a8a8f207f487d"),
          Address("253488078a4edf4d6f42f113d1e62836a942cf1a"),
          Address("86af3e9626fce1957c82e88cbf04ddf3a2ed7915"),
          Address("b136707642a4ea12fb4bae820f03d2562ebff487"),
          Address("dbe9b615a3ae8709af8b93336ce9b477e4ac0940"),
          Address("f14c14075d6c4ed84b86798af0956deef67365b5"),
          Address("ca544e5c4687d109611d0f8f928b53a25af72448"),
          Address("aeeb8ff27288bdabc0fa5ebb731b6f409507516c"),
          Address("cbb9d3703e651b0d496cdefb8b92c25aeb2171f7"),
          Address("6d87578288b6cb5549d5076a207456a1f6a63dc0"),
          Address("b2c6f0dfbb716ac562e2d85d6cb2f8d5ee87603e"),
          Address("accc230e8a6e5be9160b8cdf2864dd2a001c28b6"),
          Address("2b3455ec7fedf16e646268bf88846bd7a2319bb2"),
          Address("4613f3bca5c44ea06337a9e439fbc6d42e501d0a"),
          Address("d343b217de44030afaa275f54d31a9317c7f441e"),
          Address("84ef4b2357079cd7a7c69fd7a37cd0609a679106"),
          Address("da2fef9e4a3230988ff17df2165440f37e8b1708"),
          Address("f4c64518ea10f995918a454158c6b61407ea345c"),
          Address("7602b46df5390e432ef1c307d4f2c9ff6d65cc97"),
          Address("bb9bc244d798123fde783fcc1c72d3bb8c189413"),
          Address("807640a13483f8ac783c557fcdf27be11ea4ac7a")
        )
      }
    )
  )
  val Eip158Config = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = -1,
    eip150BlockNumber = -1,
    eip155BlockNumber = -1,
    eip160BlockNumber = -1,
    eip161BlockNumber = 0,
    maxCodeSize = Some(24576)
  )
  val ByzantiumConfig = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = -1,
    eip150BlockNumber = -1,
    eip155BlockNumber = -1,
    eip160BlockNumber = -1,
    eip161BlockNumber = -1,
    maxCodeSize = Some(24576),
    byzantiumBlockNumber = 0,
    monetaryPolicyConfig =
      MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"), BigInt("3000000000000000000"))
  )
  val ConstantinopleConfig = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = -1,
    eip150BlockNumber = -1,
    eip155BlockNumber = -1,
    eip160BlockNumber = -1,
    eip161BlockNumber = -1,
    maxCodeSize = Some(24576),
    byzantiumBlockNumber = -1,
    constantinopleBlockNumber = 0,
    monetaryPolicyConfig = MonetaryPolicyConfig(
      5000000,
      0.2,
      BigInt("5000000000000000000"),
      BigInt("3000000000000000000"),
      BigInt("2000000000000000000")
    )
  )
  val ConstantinopleFixConfig = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = -1,
    eip150BlockNumber = -1,
    eip155BlockNumber = -1,
    eip160BlockNumber = -1,
    eip161BlockNumber = -1,
    maxCodeSize = Some(24576),
    byzantiumBlockNumber = -1,
    constantinopleBlockNumber = -1,
    petersburgBlockNumber = 0,
    monetaryPolicyConfig = MonetaryPolicyConfig(
      5000000,
      0.2,
      BigInt("5000000000000000000"),
      BigInt("3000000000000000000"),
      BigInt("2000000000000000000")
    )
  )
  val IstanbulConfig = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = -1,
    eip150BlockNumber = -1,
    eip155BlockNumber = -1,
    eip160BlockNumber = -1,
    eip161BlockNumber = -1,
    maxCodeSize = Some(24576),
    byzantiumBlockNumber = -1,
    constantinopleBlockNumber = -1,
    petersburgBlockNumber = -1,
    istanbulBlockNumber = 0,
    monetaryPolicyConfig = MonetaryPolicyConfig(
      5000000,
      0.2,
      BigInt("5000000000000000000"),
      BigInt("3000000000000000000"),
      BigInt("2000000000000000000")
    )
  )
  val Eip158ToByzantiumAt5Config = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = -1,
    eip150BlockNumber = -1,
    eip155BlockNumber = -1,
    eip160BlockNumber = -1,
    eip161BlockNumber = 0,
    maxCodeSize = Some(24576),
    byzantiumBlockNumber = 5,
    monetaryPolicyConfig =
      MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"), BigInt("3000000000000000000"))
  )

  val ByzantiumToConstantinopleAt5 = BaseBlockchainConfig.copy(
    frontierBlockNumber = -1,
    homesteadBlockNumber = -1,
    eip150BlockNumber = -1,
    eip155BlockNumber = -1,
    eip160BlockNumber = -1,
    eip161BlockNumber = -1,
    maxCodeSize = Some(24576),
    byzantiumBlockNumber = 0,
    constantinopleBlockNumber = 5,
    monetaryPolicyConfig = MonetaryPolicyConfig(
      5000000,
      0.2,
      BigInt("5000000000000000000"),
      BigInt("3000000000000000000"),
      BigInt("2000000000000000000")
    )
  )
}

object Validators {
  import BlockchainTestConfig._

  val frontierValidators = ValidatorsExecutor(FrontierConfig, Protocol.Ethash)
  val homesteadValidators = ValidatorsExecutor(HomesteadConfig, Protocol.Ethash)
  val eip150Validators = ValidatorsExecutor(Eip150Config, Protocol.Ethash)
  val frontierToHomesteadValidators = ValidatorsExecutor(FrontierToHomesteadAt5, Protocol.Ethash)
  val homesteadToEipValidators = ValidatorsExecutor(HomesteadToEIP150At5, Protocol.Ethash)
  val homesteadToDaoValidators = ValidatorsExecutor(HomesteadToDaoAt5, Protocol.Ethash)
  val eip158Validators = ValidatorsExecutor(Eip158Config, Protocol.Ethash)
  val byzantiumValidators = ValidatorsExecutor(ByzantiumConfig, Protocol.Ethash)
  val constantinopleValidators = ValidatorsExecutor(ConstantinopleConfig, Protocol.Ethash)
  val constantinopleFixValidators = ValidatorsExecutor(ConstantinopleFixConfig, Protocol.Ethash)
  val istanbulValidators = ValidatorsExecutor(IstanbulConfig, Protocol.Ethash)
  val eip158ToByzantiumValidators = ValidatorsExecutor(Eip158ToByzantiumAt5Config, Protocol.Ethash)
  val byzantiumToConstantinopleAt5 = ValidatorsExecutor(ByzantiumToConstantinopleAt5, Protocol.Ethash)
}

// Connected with: https://github.com/ethereum/tests/issues/480
object ValidatorsWithSkippedPoW {

  import BlockchainTestConfig._

  val frontierValidators = ValidatorsExecutor(FrontierConfig, new EthashTestBlockHeaderValidator(FrontierConfig))
  val homesteadValidators = ValidatorsExecutor(HomesteadConfig, new EthashTestBlockHeaderValidator(HomesteadConfig))
  val eip150Validators = ValidatorsExecutor(Eip150Config, new EthashTestBlockHeaderValidator(Eip150Config))
  val frontierToHomesteadValidators =
    ValidatorsExecutor(FrontierToHomesteadAt5, new EthashTestBlockHeaderValidator(FrontierToHomesteadAt5))
  val homesteadToEipValidators =
    ValidatorsExecutor(HomesteadToEIP150At5, new EthashTestBlockHeaderValidator(HomesteadToEIP150At5))
  val homesteadToDaoValidators =
    ValidatorsExecutor(HomesteadToDaoAt5, new EthashTestBlockHeaderValidator(HomesteadToDaoAt5))
  val eip158Validators = ValidatorsExecutor(Eip158Config, new EthashTestBlockHeaderValidator(Eip158Config))
  val byzantiumValidators = ValidatorsExecutor(ByzantiumConfig, new EthashTestBlockHeaderValidator(ByzantiumConfig))
  val constantinopleValidators =
    ValidatorsExecutor(ConstantinopleConfig, new EthashTestBlockHeaderValidator(ConstantinopleConfig))
  val constantinopleFixValidators =
    ValidatorsExecutor(ConstantinopleFixConfig, new EthashTestBlockHeaderValidator(ConstantinopleFixConfig))
  val istanbulValidators = ValidatorsExecutor(IstanbulConfig, new EthashTestBlockHeaderValidator(IstanbulConfig))
  val eip158ToByzantiumValidators =
    ValidatorsExecutor(Eip158ToByzantiumAt5Config, new EthashTestBlockHeaderValidator(Eip158ToByzantiumAt5Config))
  val byzantiumToConstantinopleAt5 =
    ValidatorsExecutor(ByzantiumToConstantinopleAt5, new EthashTestBlockHeaderValidator(ByzantiumToConstantinopleAt5))
}
