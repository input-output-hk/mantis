package io.iohk.ethereum.rpcTest

import org.web3j.protocol.core.DefaultBlockParameter

object TestData {

  case class TestBlock(hash: String, number: BigInt, transactions: List[TestTransaction] = List.empty , uncles: List[TestUncle] = List.empty) {
    lazy val blockNumber: DefaultBlockParameter = DefaultBlockParameter.valueOf(number.bigInteger)
  }
  case class TestTransaction(hash: String, index: BigInt)
  case class TestUncle(hash: String, index: BigInt)
  case class TestAccount(address: String, password: String, balance: BigInt)

  // MainNet existing blocks
  val oneUncleTestBlock: TestBlock = TestBlock("0xb055593f14d994d333d4a7fbf6251c8fef00d24ca74e1355de90729ca9b6d7f5", 1024,
    uncles = List(TestUncle("0xe00ecf0381819d0b8cf86eed3e1d91db26b3e78f80bab8b388768a49210c7e6f", 0))
  )
  val noTransactionsOrUnclesBlock: TestBlock = TestBlock("0x73b20034e531f385a59401bbda9a225be12b2fd42d7c21e4c3d11b3d7be34244", 2000)
  val twoTransactionBlock: TestBlock = TestBlock("0x3152ec08cab959b3db6469104c59b803162cf37a23293e8df306e559218f5c6f", 127117,
    List(
      TestTransaction("0x1f76a07cc698c1ef425c406ede5db8fa5ed67f084397ac74fd48348bd08fbb1d", 0),
      TestTransaction("0x4fcf5556f2ebb8d47e1335641fa1b1f1fd24e41645f666fa8b16af6c03eddf19", 1)
    )
  )

  // Exisiting privatenet Accounts
  val firstAccount: TestAccount = TestAccount("0x316158e265fa708c623cc3094b2bb5889e0f5ca5", "hunter2", BigInt("100000000000000000000"))
  val secondAccount: TestAccount = TestAccount("0xb9ec69316a8810db91c36de79c4f1e785f2c35fc", "", BigInt("100000000000000000000"))

  // Account not used in value transfers
  val thirdAccount: TestAccount = TestAccount("0x488c10c91771d3b3c25f63b6414309b119baacb5", "", BigInt("100000000000000000000"))

  val gethAccount: TestAccount = TestAccount("0x03010360ebbb7f49362a2c650a67661d464e2089", "", BigInt("0"))


  val unexistingAccount: TestAccount = TestAccount("0xaaaa10c91771d3b3c25f63b6414309b119baacb5", "", BigInt("100000000000000000000"))
  val coinbase = "0x0011223344556677889900112233445566778899"
  val defaultGasPrice: BigInt = BigInt(20000000000L)
  val defaultGas: BigInt = BigInt(90000)
}
