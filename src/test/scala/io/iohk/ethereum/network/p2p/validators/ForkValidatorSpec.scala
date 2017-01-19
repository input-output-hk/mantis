package io.iohk.ethereum.network.p2p.validators

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeader, BlockHeaders}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class ForkValidatorSpec extends FlatSpec with Matchers {

  val daoForkValidator = ForkValidator(1920000, Hex.decode("94365e3a8c0b35089c1d1195081fe7489b528a84b22199c916180db8b28ade7f"))

  "DaoForkValidator" should "determine if it's ETH fork" in {
    assert(Hex.toHexString(daoForkHeader.hash) == "4985f5ca3d2afbec36529aa96f74de3cc10a2a4a6c44f2157a57d2c6059a11bb")
    val validationResult = daoForkValidator.validate(BlockHeaders(Seq(daoForkHeader)))
    assert(validationResult.isDefined)
    assert(validationResult.get.invalidHeaders.size == 1)
    assert(validationResult.get.invalidHeaders.head.hash sameElements daoForkHeader.hash)
  }

  "DaoForkValidator" should "determine if it's ETC fork" in {
    assert(Hex.toHexString(etcForkHeader.hash) == "94365e3a8c0b35089c1d1195081fe7489b528a84b22199c916180db8b28ade7f")
    assert(daoForkValidator.validate(BlockHeaders(Seq(etcForkHeader))).isEmpty)
  }

  "DaoForkValidator" should "detect invalid fork amongst other headers" in {
    val validationResult = daoForkValidator.validate(BlockHeaders(Seq(etcForkHeader, daoForkHeader, someHeader)))
    assert(validationResult.isDefined)
    assert(validationResult.get.invalidHeaders.size == 1)
    assert(validationResult.get.invalidHeaders.head.hash sameElements daoForkHeader.hash)
  }

  val someHeader: BlockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("322c6d0946950a85673b71c5a9a536ebb1d9498ad81df1a90e8b72251752ada6")),
    ommersHash = ByteString(Hex.decode("143f89e81f6319fcd4f460b28a035376dc4b7cb1cd4c9b2036a53cd6d9e47933")),
    beneficiary = ByteString(Hex.decode("bcdfc35b86bedf72f0cda046a3c16829a2ef41d1")),
    stateRoot = ByteString(Hex.decode("de32eda1f31bb9412ed4563c8392a9bbba76bc876dd5b98218b64266dd8238fd")),
    transactionsRoot = ByteString(Hex.decode("7701df8e07169452554d14aadd7bfa256d4a1d0355c1d174ab373e3e2d0a3743")),
    receiptsRoot = ByteString(Hex.decode("26cf9d9422e9dd95aedc7914db690b92bab6902f5221d62694a2fa5d065f534b")),
    logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
    difficulty = BigInt(62452008773236L),
    number = BigInt(1910000),
    gasLimit = BigInt(4700000),
    gasUsed = BigInt(0),
    unixTimestamp = 1468877583L,
    extraData = ByteString(Hex.decode("4477617266506f6f6c")),
    mixHash = ByteString(Hex.decode("7d1f1c34e6ab26c4a28923363c238c21299aafd44ac42a775ed1fb2834126b3f")),
    nonce = ByteString(Hex.decode("493fb58c01b25f6f"))
  )

  val daoForkHeader: BlockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("bcdfc35b86bedf72f0cda046a3c16829a2ef41d1")),
    stateRoot = ByteString(Hex.decode("c5e389416116e3696cce82ec4533cce33efccb24ce245ae9546a4b8f0d5e9a75")),
    transactionsRoot = ByteString(Hex.decode("7701df8e07169452554d14aadd7bfa256d4a1d0355c1d174ab373e3e2d0a3743")),
    receiptsRoot = ByteString(Hex.decode("26cf9d9422e9dd95aedc7914db690b92bab6902f5221d62694a2fa5d065f534b")),
    logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
    difficulty = BigInt(62413376722602L),
    number = BigInt(1920000),
    gasLimit = BigInt(4712384),
    gasUsed = BigInt(84000),
    unixTimestamp = 1469020840L,
    extraData = ByteString(Hex.decode("64616f2d686172642d666f726b")),
    mixHash = ByteString(Hex.decode("5b5acbf4bf305f948bd7be176047b20623e1417f75597341a059729165b92397")),
    nonce = ByteString(Hex.decode("bede87201de42426"))
  )

  val etcForkHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("61c808d82a3ac53231750dadc13c777b59310bd9")),
    stateRoot = ByteString(Hex.decode("614d7d358b03cbdaf0343529673be20ad45809d02487f023e047efdce9da8aff")),
    transactionsRoot = ByteString(Hex.decode("d33068a7f21bff5018a00ca08a3566a06be4196dfe9e39f96e431565a619d455")),
    receiptsRoot = ByteString(Hex.decode("7bda9aa65977800376129148cbfe89d35a016dd51c95d6e6dc1e76307d315468")),
    logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
    difficulty = BigInt(62413376722602L),
    number = BigInt(1920000),
    gasLimit = BigInt(4712384),
    gasUsed = BigInt(84000),
    unixTimestamp = 1469020839L,
    extraData = ByteString(Hex.decode("e4b883e5bda9e7a59ee4bb99e9b1bc")),
    mixHash = ByteString(Hex.decode("c52daa7054babe515b17ee98540c0889cf5e1595c5dd77496997ca84a68c8da1")),
    nonce = ByteString(Hex.decode("05276a600980199d"))
  )
}
