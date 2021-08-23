package io.iohk.ethereum.utils

import akka.util.ByteString

import scala.jdk.CollectionConverters._
import scala.util.Try

import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.{Config => TypesafeConfig}

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.utils.NumericUtils._

case class BlockchainConfig(
    powTargetTime: Option[Long] = None,
    forkBlockNumbers: ForkBlockNumbers,
    treasuryAddress: Address,
    maxCodeSize: Option[BigInt],
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
    allowedMinersPublicKeys: Set[ByteString] = Set.empty,
    capabilities: List[Capability] = List.empty
) {
  val minRequireSignatures: Int = (Math.floor(checkpointPubKeys.size / 2) + 1).toInt

  def withUpdatedForkBlocks(update: (ForkBlockNumbers) => ForkBlockNumbers): BlockchainConfig =
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
    difficultyBombPauseBlockNumber: BigInt,
    difficultyBombContinueBlockNumber: BigInt,
    difficultyBombRemovalBlockNumber: BigInt,
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
    ecip1099BlockNumber: BigInt,
    muirGlacierBlockNumber: BigInt,
    magnetoBlockNumber: BigInt
) {
  def all: List[BigInt] = this.productIterator.toList.flatMap {
    case i: BigInt => Some(i)
    case i: Option[_] =>
      i.flatMap {
        case n if n.isInstanceOf[BigInt] => Some(n.asInstanceOf[BigInt])
        case _                           => None
      }
    case _ => None
  }
}

object ForkBlockNumbers {
  val Empty: ForkBlockNumbers = ForkBlockNumbers(
    frontierBlockNumber = 0,
    homesteadBlockNumber = Long.MaxValue,
    difficultyBombPauseBlockNumber = Long.MaxValue,
    difficultyBombContinueBlockNumber = Long.MaxValue,
    difficultyBombRemovalBlockNumber = Long.MaxValue,
    eip106BlockNumber = Long.MaxValue,
    eip150BlockNumber = Long.MaxValue,
    eip160BlockNumber = Long.MaxValue,
    eip155BlockNumber = Long.MaxValue,
    eip161BlockNumber = Long.MaxValue,
    byzantiumBlockNumber = Long.MaxValue,
    constantinopleBlockNumber = Long.MaxValue,
    istanbulBlockNumber = Long.MaxValue,
    atlantisBlockNumber = Long.MaxValue,
    aghartaBlockNumber = Long.MaxValue,
    phoenixBlockNumber = Long.MaxValue,
    petersburgBlockNumber = Long.MaxValue,
    ecip1098BlockNumber = Long.MaxValue,
    ecip1097BlockNumber = Long.MaxValue,
    ecip1099BlockNumber = Long.MaxValue,
    ecip1049BlockNumber = None,
    muirGlacierBlockNumber = Long.MaxValue,
    magnetoBlockNumber = Long.MaxValue
  )
}

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
    val muirGlacierBlockNumber: BigInt = BigInt(blockchainConfig.getString("muir-glacier-block-number"))
    val magnetoBlockNumber: BigInt = BigInt(blockchainConfig.getString("magneto-block-number"))

    val capabilities: List[Capability] =
      blockchainConfig.getStringList("capabilities").asScala.toList.map(Capability.parseUnsafe)

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
        difficultyBombPauseBlockNumber = difficultyBombPauseBlockNumber,
        difficultyBombContinueBlockNumber = difficultyBombContinueBlockNumber,
        difficultyBombRemovalBlockNumber = difficultyBombRemovalBlockNumber,
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
        ecip1099BlockNumber = ecip1099BlockNumber,
        muirGlacierBlockNumber = muirGlacierBlockNumber,
        magnetoBlockNumber = magnetoBlockNumber
      ),
      treasuryAddress = treasuryAddress,
      maxCodeSize = maxCodeSize,
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
      allowedMinersPublicKeys = allowedMinersPublicKeys,
      capabilities = capabilities
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
