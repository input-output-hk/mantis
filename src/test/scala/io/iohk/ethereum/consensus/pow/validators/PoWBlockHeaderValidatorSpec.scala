package io.iohk.ethereum.consensus.pow.validators

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.client.RequestBuilding.Post
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.util.ByteString
import io.iohk.ethereum.consensus.pow.KeccakDataUtils
import io.iohk.ethereum.consensus.pow.KeccakDataUtils.header
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderPoWError
import io.iohk.ethereum.consensus.validators.BlockHeaderValid
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.Config
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Await
import scala.concurrent.duration.Duration


class PoWBlockHeaderValidatorSpec extends AnyFlatSpecLike with Matchers {
  import PoWBlockHeaderValidatorSpec._

  "PoWBlockHeaderValidator" should "call KeccakBlockHeaderValidator when chain is in Keccak" in {
    val keccakConfig = blockchainConfig.copy(ecip1049BlockNumber = Some(10))
    val validatorForKeccak = new PoWBlockHeaderValidator(keccakConfig)

    validatorForKeccak.validateEvenMore(validKeccakBlockHeader) shouldBe Right(BlockHeaderValid)

    // to show that indeed the right validator needs to be called
    validatorForKeccak.validateEvenMore(validEthashBlockHeader) shouldBe Left(HeaderPoWError)
  }

  it should "call EthashBlockHeaderValidator when chain is not in Keccak2" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    import org.json4s._
    import org.json4s.native.JsonMethods._
    implicit val system = ActorSystem("SingleRequest")

    implicit val formats: Formats = DefaultFormats

    //11455798

    def run(n: Int) = {
      val r: HttpRequest = Post("https://www.ethercluster.com/etc", s"""{"method":"eth_getBlockByNumber","params":["$n", false],"id":1,"jsonrpc":"2.0"}""")
      val resp = Http().singleRequest(r)

      val meh = Await.result(resp, Duration.Inf)

      val res = Unmarshal(meh).to[String]

      val dataRaw = Await.result(res, Duration.Inf)

      val result = parse(dataRaw) \ "result"

      val bh = result.extract[Data]

//      println(bh)

      val validator = new EthashBlockHeaderValidator(blockchainConfig)
      validator.validateHeader(bh.to)
    }
    run(12329998)
    run(12329999)
    run(12330000)
    run(12330001)
  }

  case class Data(
    number : String,
    hash : String,
    mixHash : String,
    parentHash : String,
    nonce : String,
    sha3Uncles : String,
    logsBloom : String,
    transactionsRoot : String,
    stateRoot : String,
    receiptsRoot : String,
    miner : String,
    difficulty : String,
    totalDifficulty : String,
    extraData : String,
    size : String,
    gasLimit : String,
    gasUsed : String,
    timestamp : String
  ) {
    def to: BlockHeader =
      BlockHeader(
        parentHash = ByteString(Hex.decode(parentHash.drop(2))),
        ommersHash = ByteString(Hex.decode(sha3Uncles.drop(2))),
        beneficiary = ByteString(Hex.decode(miner.drop(2))),
        stateRoot = ByteString(Hex.decode(stateRoot.drop(2))),
        transactionsRoot = ByteString(Hex.decode(transactionsRoot.drop(2))),
        receiptsRoot = ByteString(Hex.decode(receiptsRoot.drop(2))),
        logsBloom = ByteString(Hex.decode(logsBloom.drop(2))),
        difficulty = BigInt(difficulty.drop(2), 16),
        number = BigInt(number.drop(2), 16),
        gasLimit = BigInt(gasLimit.drop(2), 16),
        gasUsed = BigInt(gasUsed.drop(2), 16),
        unixTimestamp = BigInt(timestamp.drop(2), 16).toLong,
        extraData = ByteString(Hex.decode(extraData.drop(2))),
        mixHash = ByteString(Hex.decode(mixHash.drop(2))),
        nonce = ByteString(Hex.decode(nonce.drop(2)))
      )
  }

  it should "call EthashBlockHeaderValidator when chain is not in Keccak" in {
    val validator = new EthashBlockHeaderValidator(blockchainConfig)

    val res0 = validator.validateHeader(etcBlock1)
    println(res0)

    val res1 = validator.validateHeader(etcBlock11600008)
    println(res1)

    val res2 = validator.validateHeader(myOwnBlockHeader12455700)
    println(res2)

    val res3 = validator.validateHeader(myOwnBlockHeader12455798)
    println(res3)
    println(myOwnBlockHeader12455798.hashAsHexString)

    val res4 = validator.validateHeader(etcBlock660001)
    println(res4)
    println(etcBlock660001.hashAsHexString)

    val res5 = validator.validateHeader(etcBlock6600014)
    println(res5)
    println(etcBlock6600014.hashAsHexString)


    // to show that indeed the right validator needs to be called
//    validatorForKeccak.validateEvenMore(validKeccakBlockHeader) shouldBe Left(HeaderPoWError)
  }
}

object PoWBlockHeaderValidatorSpec {
  val blockchainConfig = Config.blockchains.blockchainConfig

  val validKeccakBlockHeader = KeccakDataUtils.header.copy(
    mixHash = ByteString(Hex.decode("d033f82e170ff16640e902fad569243c39bce9e4da948ccc298c541b34cd263b")),
    nonce = ByteString(Hex.decode("f245822d3412da7f"))
  )

  val validEthashBlockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("d882d5c210bab4cb7ef0b9f3dc2130cb680959afcd9a8f9bf83ee6f13e2f9da3")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("95f484419881c6e9b6de7fb3f8ad03763bd49a89")),
    stateRoot = ByteString(Hex.decode("634a2b20c9e02afdda7157afe384306c5acc4fb9c09b45dc0203c0fbb2fed0e6")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = BigInt("989772"),
    number = 20,
    gasLimit = 131620495,
    gasUsed = 0,
    unixTimestamp = 1486752441,
    extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
    mixHash = ByteString(Hex.decode("6bc729364c9b682cfa923ba9480367ebdfa2a9bca2a652fe975e8d5958f696dd")),
    nonce = ByteString(Hex.decode("797a8f3a494f937b"))
  )

  val myOwnBlockHeader12455799 = BlockHeader(
    parentHash = ByteString(Hex.decode("dd8d0d41fe4a3d0d49a45b42d9603c453fde4468790dcedef14c4bc22fdc3ec2")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("df7d7e053933b5cc24372f878c90e62dadad5d42")),
    stateRoot = ByteString(Hex.decode("4381ed0a6c2711c454de68280edccb946b3c03c55eba507a082470ad68b93ab3")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(
      Hex.decode(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" * 256
      )
    ),
    difficulty = BigInt("110448456679854"),
    number = 12455799,
    gasLimit = 8015541,
    gasUsed = 0,
    unixTimestamp = 1617091303,
    extraData = ByteString(Hex.decode("7374726174756d2d65752d32")),
    mixHash = ByteString(Hex.decode("2ba8e81776c7d09abe88bb0f28ea43e787ceaa6a321bfaf22580d9c8da3e3a3c")),
    nonce = ByteString(Hex.decode("a30e33ab97762a45"))
  )

  /*
  {
  "jsonrpc" : "2.0",
  "id" : 1,
  "result" : {
    "number" : "0xbe0f76",
    "hash" : "0xdd8d0d41fe4a3d0d49a45b42d9603c453fde4468790dcedef14c4bc22fdc3ec2",
    "mixHash" : "0xa3648aa609cc61c001c0fbc47cf49cdb40d0289b49a07cde6099b6610dda58b0",
    "parentHash" : "0xd9356325fb3d3831f1cf970752d98d1918ee1f6ec5cc5336b990b80fb609bc96",
    "nonce" : "0x9dcb2623b0a033cb",
    "sha3Uncles" : "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
    "logsBloom" : "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    "transactionsRoot" : "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "stateRoot" : "0xe739c693e5fd432f37275058b99cf92041b058871533111992b083b69fdaaeb5",
    "receiptsRoot" : "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "miner" : "0x0239da7f7d5af4cff574c507bb6ce18ddc73b875",
    "difficulty" : "0x6473c906c1ae",
    "totalDifficulty" : "0x3cb2a78ac27fa9bf1e",
    "extraData" : "0xe4b883e5bda9e7a59ee4bb99e9b1bc0005",
    "size" : "0x216",
    "gasLimit" : "0x7a302a",
    "gasUsed" : "0x0",
    "timestamp" : "0x6062dadd",
    "uncles" : [ ],
    "transactions" : [ ]
  }
}
   */

  // BE0F76
  val myOwnBlockHeader12455798 = BlockHeader(
    parentHash = ByteString(Hex.decode("d9356325fb3d3831f1cf970752d98d1918ee1f6ec5cc5336b990b80fb609bc96")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("0239da7f7d5af4cff574c507bb6ce18ddc73b875")),
    stateRoot = ByteString(Hex.decode("e739c693e5fd432f37275058b99cf92041b058871533111992b083b69fdaaeb5")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(
      Hex.decode(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      )
    ),
    difficulty = BigInt("6473c906c1ae", 16),
    number = BigInt("be0f76", 16),
    gasLimit = BigInt("7a302a", 16),
    gasUsed = BigInt("0", 16),
    unixTimestamp = BigInt("6062dadd", 16).toLong,
    extraData = ByteString(Hex.decode("e4b883e5bda9e7a59ee4bb99e9b1bc0005")),
    mixHash = ByteString(Hex.decode("a3648aa609cc61c001c0fbc47cf49cdb40d0289b49a07cde6099b6610dda58b0")),
    nonce = ByteString(Hex.decode("9dcb2623b0a033cb"))
  )

  /*
  {
  "jsonrpc" : "2.0",
  "id" : 1,
  "result" : {
    "number" : "0xbe0f14",
    "hash" : "0x9bf514f6779d551ac6a5a3845077065ed22d5901c7e522d8d7daed9989a421a2",
    "mixHash" : "0x4951db40b28e3f2dfea6f82f0459d65535e4f3b64ce157baca03a6780e98c24c",
    "parentHash" : "0x7316626364cd60e9cdd347a74e8682075c334ac5e985f8027d325b223410059a",
    "nonce" : "0x16d1016e5ca2add5",
    "sha3Uncles" : "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
    "logsBloom" : "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    "transactionsRoot" : "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "stateRoot" : "0x395cd5106d5106967182bf532d56bc437363ac3f56709ca16b661acb20558979",
    "receiptsRoot" : "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "miner" : "0xdf7d7e053933b5cc24372f878c90e62dadad5d42",
    "difficulty" : "0x64d92f8d2828",
    "totalDifficulty" : "0x3cb281060740b013d7",
    "extraData" : "0x7374726174756d2d65752d31",
    "size" : "0x211",
    "gasLimit" : "0x7a4f0e",
    "gasUsed" : "0x0",
    "timestamp" : "0x6062d544",
    "uncles" : [ ],
    "transactions" : [ ]
  }
}
   */

  // 0xBE0F14
  val myOwnBlockHeader12455700 = BlockHeader(
    parentHash = ByteString(Hex.decode("7316626364cd60e9cdd347a74e8682075c334ac5e985f8027d325b223410059a")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("df7d7e053933b5cc24372f878c90e62dadad5d42")),
    stateRoot = ByteString(Hex.decode("395cd5106d5106967182bf532d56bc437363ac3f56709ca16b661acb20558979")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(
      Hex.decode(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      )
    ),
    difficulty = BigInt("64d92f8d2828", 16),
    number = BigInt("be0f14", 16),
    gasLimit = BigInt("7a4f0e", 16),
    gasUsed = BigInt("0", 16),
    unixTimestamp = BigInt("6062d544", 16).toLong,
//    difficulty = BigInt("110883968460840"),
//    number = 12455700,
//    gasLimit = 8015630,
//    gasUsed = 0,
//    unixTimestamp = 1617089860,
    extraData = ByteString(Hex.decode("7374726174756d2d65752d31")),
    mixHash = ByteString(Hex.decode("4951db40b28e3f2dfea6f82f0459d65535e4f3b64ce157baca03a6780e98c24c")),
    nonce = ByteString(Hex.decode("16d1016e5ca2add5"))
  )

  /*
  {
  "jsonrpc" : "2.0",
  "id" : 1,
  "result" : {
    "number" : "0x1",
    "hash" : "0x88e96d4537bea4d9c05d12549907b32561d3bf31f45aae734cdc119f13406cb6",
    "mixHash" : "0x969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59",
    "parentHash" : "0xd4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3",
    "nonce" : "0x539bd4979fef1ec4",
    "sha3Uncles" : "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
    "logsBloom" : "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    "transactionsRoot" : "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "stateRoot" : "0xd67e4d450343046425ae4271474353857ab860dbc0a1dde64b41b5cd3a532bf3",
    "receiptsRoot" : "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "miner" : "0x05a56e2d52c817161883f50c441c3228cfe54d9f",
    "difficulty" : "0x3ff800000",
    "totalDifficulty" : "0x7ff800000",
    "extraData" : "0x476574682f76312e302e302f6c696e75782f676f312e342e32",
    "size" : "0x219",
    "gasLimit" : "0x1388",
    "gasUsed" : "0x0",
    "timestamp" : "0x55ba4224",
    "uncles" : [ ],
    "transactions" : [ ]
  }
}
   */
  val etcBlock1 =  BlockHeader(
    parentHash = ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("05a56e2d52c817161883f50c441c3228cfe54d9f")),
    stateRoot = ByteString(Hex.decode("d67e4d450343046425ae4271474353857ab860dbc0a1dde64b41b5cd3a532bf3")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(
      Hex.decode(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      )
    ),
    difficulty = BigInt("3ff800000", 16),
    number = BigInt("1", 16),
    gasLimit = BigInt("1388", 16),
    gasUsed = BigInt("0", 16),
    unixTimestamp = BigInt("55ba4224", 16).toLong,
    extraData = ByteString(Hex.decode("476574682f76312e302e302f6c696e75782f676f312e342e32")),
    mixHash = ByteString(Hex.decode("969b900de27b6ac6a67742365dd65f55a0526c41fd18e1b16f1a1215c2e66f59")),
    nonce = ByteString(Hex.decode("539bd4979fef1ec4"))
  )

  /*
{
  "jsonrpc" : "2.0",
  "id" : 1,
  "result" : {
    "number" : "b10088",
    "hash" : "95a3b92bc7e10b173b1e3d49229f421f5d1a65eaf0d1ec0bf4e580efa3fbe2e9",
    "mixHash" : "15f438734c5ca5eca93ae04112efd00641f5038c80efa4b05ee853750b225776",
    "parentHash" : "49e569e972ba1d7a332ab6016c64e476df454e2d467155a309a0a06fbe82af64",
    "nonce" : "5fdacd59046bc80b",
    "sha3Uncles" : "1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
    "logsBloom" : "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    "transactionsRoot" : "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "stateRoot" : "037ff65646aad9157d6e36deb6cb76b11db37c33657dbf06371f6dcb020b6da9",
    "receiptsRoot" : "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "miner" : "df7d7e053933b5cc24372f878c90e62dadad5d42",
    "difficulty" : "260a3ffe7562",
    "totalDifficulty" : "383fc62ff46e170f63",
    "extraData" : "7374726174756d2d65752d31",
    "size" : "211",
    "gasLimit" : "7a3083",
    "gasUsed" : "0",
    "timestamp" : "5faeca1b",
    "uncles" : [ ],
    "transactions" : [ ]
  }
}
   */

  val etcBlock11600008 =  BlockHeader(
    parentHash = ByteString(Hex.decode("49e569e972ba1d7a332ab6016c64e476df454e2d467155a309a0a06fbe82af64")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("df7d7e053933b5cc24372f878c90e62dadad5d42")),
    stateRoot = ByteString(Hex.decode("037ff65646aad9157d6e36deb6cb76b11db37c33657dbf06371f6dcb020b6da9")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(
      Hex.decode(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      )
    ),
    difficulty = BigInt("260a3ffe7562", 16),
    number = BigInt("b10088", 16),
    gasLimit = BigInt("7a3083", 16),
    gasUsed = BigInt("0", 16),
    unixTimestamp = BigInt("5faeca1b", 16).toLong,
    extraData = ByteString(Hex.decode("476574682f76312e302e302f6c696e75782f676f312e342e32")),
    mixHash = ByteString(Hex.decode("15f438734c5ca5eca93ae04112efd00641f5038c80efa4b05ee853750b225776")),
    nonce = ByteString(Hex.decode("7374726174756d2d65752d31")),
  )

  /*
  {
  "jsonrpc" : "2.0",
  "id" : 1,
  "result" : {
    "number" : "a1221",
    "hash" : "8fa303778585cd9d8bc2c6a97370e05125e89f1b946c89d3e3f98c4ee1d99b03",
    "mixHash" : "fe621dea9def81adebe30a311a248cb21f979331e67d6a85d1147cd05c65ef94",
    "parentHash" : "8cff63daa01cbe0c61b72a9a92602e2dfe350b058a8060713e446d46ad303422",
    "nonce" : "59da0e58f447457d",
    "sha3Uncles" : "1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
    "logsBloom" : "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    "transactionsRoot" : "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "stateRoot" : "3d9c7fe77939bebfb0a7d120d448871e0dd6aba7e5887dafbfe4d857d55cffff",
    "receiptsRoot" : "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "miner" : "48737ba7856508d9a9674e55d1743f412716cb8f",
    "difficulty" : "6d78f0ba4e1",
    "totalDifficulty" : "36c6a1920887dba4",
    "extraData" : "d983010302844765746887676f312e352e318777696e646f7773",
    "size" : "21f",
    "gasLimit" : "2fefd8",
    "gasUsed" : "0",
    "timestamp" : "5666da5c",
    "uncles" : [ ],
    "transactions" : [ ]
  }
}
   */
  val etcBlock660001 =  BlockHeader(
    parentHash = ByteString(Hex.decode("8cff63daa01cbe0c61b72a9a92602e2dfe350b058a8060713e446d46ad303422")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("48737ba7856508d9a9674e55d1743f412716cb8f")),
    stateRoot = ByteString(Hex.decode("3d9c7fe77939bebfb0a7d120d448871e0dd6aba7e5887dafbfe4d857d55cffff")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(
      Hex.decode(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      )
    ),
    difficulty = BigInt("6d78f0ba4e1", 16),
    number = BigInt("a1221", 16),
    gasLimit = BigInt("2fefd8", 16),
    gasUsed = BigInt("0", 16),
    unixTimestamp = BigInt("5666da5c", 16).toLong,
    extraData = ByteString(Hex.decode("d983010302844765746887676f312e352e318777696e646f7773")),
    mixHash = ByteString(Hex.decode("fe621dea9def81adebe30a311a248cb21f979331e67d6a85d1147cd05c65ef94")),
    nonce = ByteString(Hex.decode("59da0e58f447457d")),
  )

  /*
  {
  "jsonrpc" : "2.0",
  "id" : 1,
  "result" : {
    "number" : "64b54e",
    "hash" : "ae2f949a29568ebd9314f2832dec3a26704141302400c5a44179881cd1e20b1a",
    "mixHash" : "c850283b2139361b494c7deed2ffb388581adc1ad0dcdd4930b5d94ca4209d36",
    "parentHash" : "5dd6e4cf93a5eb26c55d9b1458cfd1d70012529b62af3bb40406b4ddc40b9506",
    "nonce" : "3116f610037e954e",
    "sha3Uncles" : "1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
    "logsBloom" : "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
    "transactionsRoot" : "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "stateRoot" : "4520ecd93a0fc669121604bd250c7c170d1c2230cecc2fe62803755f228f3380",
    "receiptsRoot" : "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
    "miner" : "24eeccf68b9772d60f2b5acabe6bb4c3221c302b",
    "difficulty" : "ba67a41ca541",
    "totalDifficulty" : "1751b6249e22453af2",
    "extraData" : "7761746572686f6c652e696f",
    "size" : "211",
    "gasLimit" : "47ef59",
    "gasUsed" : "0",
    "timestamp" : "5ba4f992",
    "uncles" : [ ],
    "transactions" : [ ]
  }
}
   */
  val etcBlock6600014 =  BlockHeader(
    parentHash = ByteString(Hex.decode("5dd6e4cf93a5eb26c55d9b1458cfd1d70012529b62af3bb40406b4ddc40b9506")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = ByteString(Hex.decode("24eeccf68b9772d60f2b5acabe6bb4c3221c302b")),
    stateRoot = ByteString(Hex.decode("4520ecd93a0fc669121604bd250c7c170d1c2230cecc2fe62803755f228f3380")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(
      Hex.decode(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      )
    ),
    difficulty = BigInt("ba67a41ca541", 16),
    number = BigInt("64b54e", 16),
    gasLimit = BigInt("47ef59", 16),
    gasUsed = BigInt("0", 16),
    unixTimestamp = BigInt("5ba4f992", 16).toLong,
    extraData = ByteString(Hex.decode("7761746572686f6c652e696f")),
    mixHash = ByteString(Hex.decode("c850283b2139361b494c7deed2ffb388581adc1ad0dcdd4930b5d94ca4209d36")),
    nonce = ByteString(Hex.decode("3116f610037e954e")),
  )
}
