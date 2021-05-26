package io.iohk.ethereum.utils

import akka.util.ByteString
import com.typesafe.config.{Config => TypesafeConfig}
import io.iohk.ethereum.domain.{Address, UInt256}
import io.iohk.ethereum.utils.NumericUtils._

import scala.jdk.CollectionConverters._
import scala.util.Try
import com.typesafe.config.ConfigRenderOptions

case class BlockchainConfig(
    powTargetTime: Option[Long] = None,
    forkBlockNumbers: ForkBlockNumbers,
    treasuryAddress: Address,
    maxCodeSize: Option[BigInt],
    difficultyBombPauseBlockNumber: BigInt,
    difficultyBombContinueBlockNumber: BigInt,
    difficultyBombRemovalBlockNumber: BigInt,
    customGenesisFileOpt: Option[String],
    customGenesisJsonOpt: Option[String],
    daoForkConfig: Option[DaoForkConfig],
    accountStartNonce: UInt256,
    chainId: Byte,
    networkId: Int,
    monetaryPolicyConfig: MonetaryPolicyConfig,
    gasTieBreaker: Boolean,
    ethCompatibleStorage: Boolean,
    bootstrapNodes: Set[String],
    checkpointPubKeys: Set[ByteString] = Set.empty,
    allowedMinersPublicKeys: Set[ByteString] = Set.empty
) {
  val minRequireSignatures: Int = (Math.floor(checkpointPubKeys.size / 2) + 1).toInt

  def withUpdatedForkBlocks(update: (ForkBlockNumbers) => ForkBlockNumbers) =
    copy(forkBlockNumbers = update(forkBlockNumbers))
}

case class ForkBlockNumbers(
    frontierBlockNumber: BigInt,
    homesteadBlockNumber: BigInt,
    eip106BlockNumber: BigInt,
    eip150BlockNumber: BigInt,
    eip155BlockNumber: BigInt,
    eip160BlockNumber: BigInt,
    eip161BlockNumber: BigInt,
    byzantiumBlockNumber: BigInt,
    constantinopleBlockNumber: BigInt,
    istanbulBlockNumber: BigInt,
    atlantisBlockNumber: BigInt,
    aghartaBlockNumber: BigInt,
    phoenixBlockNumber: BigInt,
    petersburgBlockNumber: BigInt,
    ecip1098BlockNumber: BigInt,
    ecip1097BlockNumber: BigInt,
    ecip1049BlockNumber: Option[BigInt],
    ecip1099BlockNumber: BigInt
)

object BlockchainConfig {

  // scalastyle:off method.length
  def fromRawConfig(blockchainConfig: TypesafeConfig): BlockchainConfig = {
    val powTargetTime: Option[Long] =
      ConfigUtils
        .getOptionalValue(blockchainConfig, _.getDuration, "pow-target-time")
        .map(_.getSeconds)
    val frontierBlockNumber: BigInt = BigInt(blockchainConfig.getString("frontier-block-number"))
    val homesteadBlockNumber: BigInt = BigInt(blockchainConfig.getString("homestead-block-number"))
    val eip106BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip106-block-number"))
    val eip150BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip150-block-number"))
    val eip155BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip155-block-number"))
    val eip160BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip160-block-number"))
    val eip161BlockNumber: BigInt = BigInt(blockchainConfig.getString("eip161-block-number"))
    val byzantiumBlockNumber: BigInt = BigInt(blockchainConfig.getString("byzantium-block-number"))
    val constantinopleBlockNumber: BigInt = BigInt(blockchainConfig.getString("constantinople-block-number"))
    val istanbulBlockNumber: BigInt = BigInt(blockchainConfig.getString("istanbul-block-number"))

    val atlantisBlockNumber: BigInt = BigInt(blockchainConfig.getString("atlantis-block-number"))
    val aghartaBlockNumber: BigInt = BigInt(blockchainConfig.getString("agharta-block-number"))
    val phoenixBlockNumber: BigInt = BigInt(blockchainConfig.getString("phoenix-block-number"))
    val petersburgBlockNumber: BigInt = BigInt(blockchainConfig.getString("petersburg-block-number"))
    val treasuryAddress = Address(blockchainConfig.getString("treasury-address"))
    val ecip1098BlockNumber: BigInt = BigInt(blockchainConfig.getString("ecip1098-block-number"))
    val ecip1097BlockNumber: BigInt = BigInt(blockchainConfig.getString("ecip1097-block-number"))
    val ecip1049BlockNumber: Option[BigInt] =
      Try(blockchainConfig.getString("ecip1049-block-number")).map(BigInt(_)).toOption

    val maxCodeSize: Option[BigInt] = Try(BigInt(blockchainConfig.getString("max-code-size"))).toOption
    val difficultyBombPauseBlockNumber: BigInt = BigInt(
      blockchainConfig.getString("difficulty-bomb-pause-block-number")
    )
    val difficultyBombContinueBlockNumber: BigInt = BigInt(
      blockchainConfig.getString("difficulty-bomb-continue-block-number")
    )
    val difficultyBombRemovalBlockNumber: BigInt = BigInt(
      blockchainConfig.getString("difficulty-bomb-removal-block-number")
    )
    val customGenesisFileOpt: Option[String] = Try(blockchainConfig.getString("custom-genesis-file")).toOption
    val customGenesisJsonOpt: Option[String] = Try(
      blockchainConfig.getObject("custom-genesis-file").render(ConfigRenderOptions.concise())
    ).toOption

    val daoForkConfig = Try(blockchainConfig.getConfig("dao")).toOption.map(DaoForkConfig(_))
    val accountStartNonce: UInt256 = UInt256(BigInt(blockchainConfig.getString("account-start-nonce")))

    val chainId: Byte = {
      val s = blockchainConfig.getString("chain-id")
      val n = parseHexOrDecNumber(s)
      require(n >= 0 && n <= 127, "chain-id must be a number in range [0, 127]")
      n.toByte
    }

    val networkId: Int = blockchainConfig.getInt("network-id")

    val monetaryPolicyConfig = MonetaryPolicyConfig(blockchainConfig.getConfig("monetary-policy"))

    val gasTieBreaker: Boolean = blockchainConfig.getBoolean("gas-tie-breaker")

    val ethCompatibleStorage: Boolean = blockchainConfig.getBoolean("eth-compatible-storage")

    val bootstrapNodes: Set[String] = blockchainConfig.getStringList("bootstrap-nodes").asScala.toSet
    val checkpointPubKeys = readPubKeySet(blockchainConfig, "checkpoint-public-keys")
    val allowedMinersPublicKeys = readPubKeySet(blockchainConfig, "allowed-miners")

    val ecip1099BlockNumber: BigInt = BigInt(blockchainConfig.getString("ecip1099-block-number"))

    BlockchainConfig(
      powTargetTime = powTargetTime,
      forkBlockNumbers = ForkBlockNumbers(
        frontierBlockNumber = frontierBlockNumber,
        homesteadBlockNumber = homesteadBlockNumber,
        eip106BlockNumber = eip106BlockNumber,
        eip150BlockNumber = eip150BlockNumber,
        eip155BlockNumber = eip155BlockNumber,
        eip160BlockNumber = eip160BlockNumber,
        eip161BlockNumber = eip161BlockNumber,
        byzantiumBlockNumber = byzantiumBlockNumber,
        constantinopleBlockNumber = constantinopleBlockNumber,
        istanbulBlockNumber = istanbulBlockNumber,
        atlantisBlockNumber = atlantisBlockNumber,
        aghartaBlockNumber = aghartaBlockNumber,
        phoenixBlockNumber = phoenixBlockNumber,
        petersburgBlockNumber = petersburgBlockNumber,
        ecip1098BlockNumber = ecip1098BlockNumber,
        ecip1097BlockNumber = ecip1097BlockNumber,
        ecip1049BlockNumber = ecip1049BlockNumber,
        ecip1099BlockNumber = ecip1099BlockNumber
      ),
      treasuryAddress = treasuryAddress,
      maxCodeSize = maxCodeSize,
      difficultyBombPauseBlockNumber = difficultyBombPauseBlockNumber,
      difficultyBombContinueBlockNumber = difficultyBombContinueBlockNumber,
      difficultyBombRemovalBlockNumber = difficultyBombRemovalBlockNumber,
      customGenesisFileOpt = customGenesisFileOpt,
      customGenesisJsonOpt = customGenesisJsonOpt,
      daoForkConfig = daoForkConfig,
      accountStartNonce = accountStartNonce,
      chainId = chainId,
      networkId = networkId,
      monetaryPolicyConfig = monetaryPolicyConfig,
      gasTieBreaker = gasTieBreaker,
      ethCompatibleStorage = ethCompatibleStorage,
      bootstrapNodes = bootstrapNodes,
      checkpointPubKeys = checkpointPubKeys,
      allowedMinersPublicKeys = allowedMinersPublicKeys
    )
  }
  // scalastyle:on method.length
  private def readPubKeySet(blockchainConfig: TypesafeConfig, path: String): Set[ByteString] = {
    val keys: Seq[String] = ConfigUtils
      .getOptionalValue(blockchainConfig, _.getStringList, path)
      .map(_.asScala.toSeq)
      .getOrElse(Nil)
    keys.map(ByteStringUtils.string2hash).toSet
  }
}
