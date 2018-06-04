package io.iohk.ethereum.rpcTest

import java.math.BigInteger
import java.security.SecureRandom

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.TransactionRequest
import io.iohk.ethereum.keystore.{KeyStoreImpl, Wallet}
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rpcTest.Tags.{MainNet, PrivNet}
import io.iohk.ethereum.utils.{KeyStoreConfig, Logger}
import org.bouncycastle.util.encoders.Hex
import org.scalatest.{FlatSpec, Matchers}
import org.web3j.protocol.admin.Admin
import org.web3j.protocol.core.DefaultBlockParameter
import org.web3j.protocol.core.methods.request.{EthFilter, Transaction}
import org.web3j.protocol.core.methods.response.EthBlock.{TransactionHash, TransactionObject}
import org.web3j.protocol.http.HttpService
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc

import scala.collection.JavaConverters._
import scala.language.implicitConversions

class RpcApiTests extends FlatSpec with Matchers with Logger {

  "Json rpc service" should "be listening before all tests" taggedAs(MainNet, PrivNet) in new ScenarioSetup {
    val response = service.netListening().send()
    response.isListening shouldBe true
  }

  it should "return correct protocol version" taggedAs(MainNet, PrivNet) in new ScenarioSetup {
    val response = service.ethProtocolVersion().send()
    hexToBigInt(response.getProtocolVersion) shouldEqual 63
  }

  it should "return correct client version" taggedAs(MainNet, PrivNet) in new ScenarioSetup {
    val response = service.web3ClientVersion().send()
    response.getWeb3ClientVersion shouldEqual "mantis/v0.1"
  }

  it should "correctly calculate Sha3" taggedAs(MainNet, PrivNet) in new ScenarioSetup {
    val response = service.web3Sha3("").send()
    response.getResult shouldEqual "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"


    var key = "000000000000000000000000316158e265fa708c623cc3094b2bb5889e0f5ca5" + "0000000000000000000000000000000000000000000000000000000000000001"
  }

  it should "eth_getBlockTransactionCountByHash" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethGetBlockTransactionCountByHash(noTransactionsOrUnclesBlock.hash).send()
    response.getTransactionCount.asBigInt shouldEqual noTransactionsOrUnclesBlock.transactions.size

    val response1 = service.ethGetBlockTransactionCountByHash(twoTransactionBlock.hash).send()
    response1.getTransactionCount.asBigInt shouldEqual twoTransactionBlock.transactions.size

    val response2 = service.ethGetBlockTransactionCountByHash(unexisitingBlockHash).send()
    response2.getResult shouldBe null
  }

  it should "eth_getBlockTransactionCountByNumber" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethGetBlockTransactionCountByNumber(noTransactionsOrUnclesBlock.blockNumber).send()
    response.getTransactionCount.asBigInt shouldEqual noTransactionsOrUnclesBlock.transactions.size

    val response1 = service.ethGetBlockTransactionCountByNumber(twoTransactionBlock.blockNumber).send()
    response1.getTransactionCount.asBigInt shouldEqual twoTransactionBlock.transactions.size

    val response2 = service.ethGetBlockTransactionCountByNumber(futureBlock).send()
    response2.getResult shouldBe null
    response2.getError.getCode shouldEqual -32602
  }

  it should "eth_getUncleCountByBlockHash" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethGetUncleCountByBlockHash(noTransactionsOrUnclesBlock.hash).send()
    response.getUncleCount.asBigInt shouldEqual 0

    val response1 = service.ethGetUncleCountByBlockHash(oneUncleTestBlock.hash).send()
    response1.getUncleCount.asBigInt shouldEqual 1

    val response2 = service.ethGetUncleCountByBlockHash(unexisitingBlockHash).send()
    response2.getResult shouldBe null
    response2.getError.getCode shouldEqual -32602
  }

  it should "eth_getUncleCountByBlockNumber" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethGetUncleCountByBlockNumber(noTransactionsOrUnclesBlock.blockNumber).send()
    response.getUncleCount.asBigInt shouldEqual 0

    val response1 = service.ethGetUncleCountByBlockNumber(oneUncleTestBlock.blockNumber).send()
    response1.getUncleCount.asBigInt shouldEqual 1

    val response2 = service.ethGetUncleCountByBlockNumber(futureBlock).send()
    response2.getResult shouldBe null
    response2.getError.getCode shouldEqual -32602

  }

  it should "eth_getBlockByHash" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethGetBlockByHash(twoTransactionBlock.hash, false).send()
    response.getBlock.getNumber.asBigInt shouldEqual twoTransactionBlock.number

    val response1 = service.ethGetBlockByHash(unexisitingBlockHash, false).send()
    response1.getBlock shouldEqual null
    response1.getError shouldEqual null

    val response2 = service.ethGetBlockByHash(badHash, false).send()
    response2.getBlock shouldEqual null
    response2.getError.getCode shouldEqual -32602
  }

  it should "eth_getTransactionByBlockHashAndIndex" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethGetTransactionByBlockHashAndIndex(twoTransactionBlock.hash, twoTransactionBlock.transactions.head.index).send()
    response.getTransaction.isPresent shouldBe true
    response.getTransaction.get().getHash shouldEqual twoTransactionBlock.transactions.head.hash


    val response1 = service.ethGetTransactionByBlockHashAndIndex(unexisitingBlockHash, 0).send()
    response1.getTransaction.isPresent shouldEqual false
    response1.getError shouldEqual null

    val response2 = service.ethGetTransactionByBlockHashAndIndex(twoTransactionBlock.hash, twoTransactionBlock.transactions.size + 1).send()
    response2.getTransaction.isPresent shouldEqual false
    response2.getError shouldEqual null
  }

  it should "eth_getTransactionByBlockNumberAndIndex" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethGetTransactionByBlockNumberAndIndex(twoTransactionBlock.blockNumber, twoTransactionBlock.transactions.head.index).send()
    response.getTransaction.isPresent shouldBe true
    response.getTransaction.get().getHash shouldEqual twoTransactionBlock.transactions.head.hash


    val response1 = service.ethGetTransactionByBlockNumberAndIndex(futureBlock, 0).send()
    response1.getTransaction.isPresent shouldEqual false
    response1.getError shouldEqual null

    val response2 = service.ethGetTransactionByBlockNumberAndIndex(twoTransactionBlock.blockNumber, twoTransactionBlock.transactions.size + 1).send()
    response2.getTransaction.isPresent shouldEqual false
    response2.getError shouldEqual null
  }

  it should "eth_getUncleByBlockHashAndIndex" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethGetUncleByBlockHashAndIndex(oneUncleTestBlock.hash, oneUncleTestBlock.uncles.head.index).send()
    response.getBlock.getHash shouldEqual  oneUncleTestBlock.uncles.head.hash

    val response1 = service.ethGetUncleByBlockHashAndIndex(unexisitingBlockHash, 0).send()
    response1.getBlock shouldEqual null
    response1.getError shouldEqual null

    val response2 = service.ethGetUncleByBlockHashAndIndex(oneUncleTestBlock.hash, oneUncleTestBlock.uncles.head.index + 1).send()
    response2.getBlock shouldEqual null
    response2.getError shouldEqual null
  }

  it should "eth_getBlockByNumber without mining" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethGetBlockByNumber(latestBlock, false).send()
    val lBlock = response.getBlock
    lBlock should not equal null

    val response1 = service.ethGetBlockByNumber(pendingBlock, false).send()
    val pBlock = response1.getBlock
    pBlock should not equal null

    lBlock.getHash shouldEqual pBlock.getHash
  }

  it should "eth_getUncleByBlockNumberAndIndex" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethGetUncleByBlockNumberAndIndex(oneUncleTestBlock.blockNumber, oneUncleTestBlock.uncles.head.index).send()
    response.getBlock.getHash shouldEqual  oneUncleTestBlock.uncles.head.hash

    val response1 = service.ethGetUncleByBlockNumberAndIndex(futureBlock, 0).send()
    response1.getBlock shouldEqual null
    response1.getError shouldEqual null

    val response2 = service.ethGetUncleByBlockNumberAndIndex(oneUncleTestBlock.blockNumber, oneUncleTestBlock.uncles.head.index + 1).send()
    response2.getBlock shouldEqual null
    response2.getError shouldEqual null
  }

  it should "eth_mining false on MainNet" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethMining().send()
    response.isMining shouldEqual false
  }

  it should "eth_hashrate equal to 0 on MainNet" taggedAs(MainNet) in new ScenarioSetup {
    val response = service.ethHashrate().send()
    response.getHashrate.asBigInt shouldEqual 0
  }

  it should "eth_getBalance" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.ethGetBalance(thirdAccount.address, latestBlock).send()
    response.getBalance.asBigInt shouldEqual thirdAccount.balance

    val response1 = service.ethGetBalance(unexistingAccount.address, latestBlock).send()
    response1.getBalance.asBigInt shouldEqual BigInt(0)
  }

  it should "eth_BlockNumber" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.ethBlockNumber().send()
    response.getBlockNumber should not equal null
    val receivedNumber = response.getBlockNumber.asBigInt

    // Wait for new block
    val minedBlock1 = service.blockObservable(false).toBlocking.first()

    val response1 = service.ethBlockNumber().send()
    response1.getBlockNumber should not equal null
    response1.getBlockNumber.asBigInt should be > receivedNumber
  }

  it should "eth_mining true on privNet" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.ethMining().send()
    response.isMining shouldEqual true
  }

  it should "eth_coinbase on privNet" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.ethCoinbase().send()
    response.getAddress shouldEqual coinbase
  }

  it should "eth_hashrate larger than 0 on privNet" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.ethHashrate().send()
    response.getHashrate.asBigInt should not equal 0
  }

  it should "personal_unlockAccount for indefinite duration" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.personalUnlockAccount(firstAccount.address, firstAccount.password, BigInt(0)).send()
    response.accountUnlocked() shouldEqual true
  }

  it should "eth_getBlockByNumber with mining" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.ethGetBlockByNumber(latestBlock, false).send()
    val lBlock = response.getBlock
    lBlock should not equal null

    val response3 = service.ethGetBlockByNumber(pendingBlock, false).send()
    val pBlock = response3.getBlock
    pBlock should not equal null


    val response1 = service.ethGetBlockByNumber(DefaultBlockParameter.valueOf(lBlock.getNumber), false).send()
    val n = response1.getBlock
    n should not equal null

    lBlock.getHash shouldEqual n.getHash

    val response2 = service.ethGetBlockByNumber(earliestBlock, false).send()
    val eBlock = response2.getBlock
    eBlock should not equal null

    eBlock.getNumber.asBigInt shouldEqual 0


    val response4 = service.ethSendTransaction(sampleTransaction).send()
    response4.getTransactionHash should not equal null
    val tHash = response4.getTransactionHash

    // Wait for the transaction to be mined
    val minedTransaction = service.transactionObservable().toBlocking.first()
    minedTransaction.getHash shouldEqual tHash

    val response5 = service.ethGetBlockByNumber(DefaultBlockParameter.valueOf(minedTransaction.getBlockNumber), false).send()
    val transaction1 = response5.getBlock.getTransactions.get(0)
    transaction1 shouldBe a[TransactionHash]
    transaction1.asInstanceOf[TransactionHash].get() shouldEqual minedTransaction.getHash

    val response6 = service.ethGetBlockByNumber(DefaultBlockParameter.valueOf(minedTransaction.getBlockNumber), true).send()
    val transaction2 = response6.getBlock.getTransactions.get(0)
    transaction2 shouldBe a[TransactionObject]
    transaction2.asInstanceOf[TransactionObject].get().getHash shouldEqual minedTransaction.getHash
    transaction2.asInstanceOf[TransactionObject].get().getFrom shouldEqual minedTransaction.getFrom

    val response7 = service.ethGetBlockByNumber(futureBlock, false).send()
    response7.getBlock shouldEqual null
  }

  it should "not send eth_sendTransaction from locked account" taggedAs(PrivNet) in new ScenarioSetup {
    val response1 = service.ethSendTransaction(new Transaction (
      thirdAccount.address,
      null,
      null,
      null,
      null,
      null,
      testContract
    )).send()

    response1.getTransactionHash shouldEqual null
    response1.getError should not equal null
    response1.getError.getMessage shouldEqual "account is locked or unknown"
  }

  it should "eth_sendTransaction for value transfer" taggedAs(PrivNet) in new ScenarioSetup {
    val transferAmaunt = 100
    val firstAccountstartBalance = service.ethGetBalance(firstAccount.address, latestBlock).send().getBalance.asBigInt
    val secondAccountstartBalance = service.ethGetBalance(secondAccount.address, latestBlock).send().getBalance.asBigInt


    val response1 = service.ethSendTransaction(valueTransfer(firstAccount.address, secondAccount.address, transferAmaunt)).send()
    response1.getError shouldEqual null

    val mined = service.transactionObservable().toBlocking.first()
    val receipt = service.ethGetTransactionReceipt(response1.getTransactionHash).send().getTransactionReceipt
    receipt.isPresent shouldEqual true

    val transactionCost = receipt.get().getCumulativeGasUsed.asBigInt * mined.getGasPrice.asBigInt

    val firstAccountEndBalance = service.ethGetBalance(firstAccount.address, latestBlock).send().getBalance.asBigInt
    val secondAccountEndBalance = service.ethGetBalance(secondAccount.address, latestBlock).send().getBalance.asBigInt

    firstAccountstartBalance - transferAmaunt - transactionCost shouldEqual firstAccountEndBalance
    secondAccountstartBalance + transferAmaunt shouldEqual secondAccountEndBalance

  }

  it should "eth_sendTransaction with several transactions in pool with same nonce" taggedAs(PrivNet) in new ScenarioSetup {
    val fundingAmount = 20000000000000000L
    val amount1 = 100
    val amount2 = 200
    val amount3 = 300
    val response =  service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val txCount =response.getTransactionCount.asBigInt

    val transfer = service.ethSendTransaction(valueTransfer(firstAccount.address, secondAccount.address, amount1, Some(txCount))).send()
    transfer.getError shouldEqual null

    val transfer1 = service.ethSendTransaction(valueTransfer(firstAccount.address, secondAccount.address, amount2, Some(txCount))).send()
    transfer1.getError shouldEqual null

    val mined = service.transactionObservable().toBlocking.first()

    mined.getValue.asBigInt shouldEqual amount2

    val response2 =  service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    response2.getTransactionCount.asBigInt shouldEqual txCount + 1

    // Transactions from two accounts with same nonce
    val (firstAcc, secondAcc) = setupTwoNewAccounts(firstAccount.address, fundingAmount)

    val transfer3 = service.ethSendTransaction(valueTransfer(firstAcc.address, secondAccount.address, amount1, Some(0))).send()
    transfer3.getError shouldEqual null

    val transfer4 = service.ethSendTransaction(valueTransfer(secondAcc.address, secondAccount.address, amount2, Some(0))).send()
    transfer4.getError shouldEqual null
    val t4hash = transfer4.getTransactionHash

    val transfer5 = service.ethSendTransaction(valueTransfer(firstAcc.address, secondAccount.address, amount3, Some(0))).send()
    transfer5.getError shouldEqual null
    val t5hash = transfer5.getTransactionHash

    val mineBlock = service.blockObservable(false).toBlocking.first()

    mineBlock.getBlock.getTransactions.size() shouldEqual 2

    val minedhashes = mineBlock.getBlock.getTransactions.asScala.map(result => result.asInstanceOf[TransactionHash].get()).toList
    minedhashes should contain theSameElementsAs List(t4hash, t5hash)
  }

  it should "eth_sendRawTransaction" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val nextNonce = response.getTransactionCount.asBigInt

    val value = 100
    val rawValueTransaction = prepareRawTx(firstAccount, toAccount = Some(secondAccount), value= Some(value), nonce = nextNonce)
    val response1 = service.ethSendRawTransaction(rawValueTransaction).send
    val txHash = response1.getTransactionHash

    val minedTransaction = service.transactionObservable().toBlocking.first()
    minedTransaction.getHash shouldEqual txHash

    val contractCreation = prepareRawTx(firstAccount, data = Some(ByteString(decode(storageContract))), nonce = nextNonce + 1)
    val response2 = service.ethSendRawTransaction(contractCreation).send
    val txHash1 = response2.getTransactionHash

    val minedTransaction1 = service.transactionObservable().toBlocking.first()
    minedTransaction1.getHash shouldEqual txHash1

    val value1 = 200
    val value2 = 300

    // two transactions with same nonce, only second will get included in blockchain
    val rawValueTransaction1 = prepareRawTx(firstAccount, toAccount = Some(secondAccount), value = Some(value1), nonce = nextNonce + 2)
    val rawValueTransaction2 = prepareRawTx(firstAccount, toAccount = Some(secondAccount), value = Some(value2), nonce = nextNonce + 2)

    val response3 = service.ethSendRawTransaction(rawValueTransaction1).send()
    response3.getError shouldEqual null

    val response4 = service.ethSendRawTransaction(rawValueTransaction2).send()
    response4.getError shouldEqual null

    val minedTransaction2 = service.transactionObservable().toBlocking.first()

    minedTransaction2.getValue.asBigInt shouldEqual value2

    val fundingAmount = 20000000000000000L
    // Transactions from two accounts with same nonce
    val (firstAcc, secondAcc) = setupTwoNewAccounts(firstAccount.address, fundingAmount)
    val rawValueTransaction3 = prepareRawTx(firstAcc, toAccount = Some(secondAccount), value = Some(value), nonce = 0)
    val rawValueTransaction4 = prepareRawTx(secondAcc, toAccount = Some(secondAccount), value = Some(value1), nonce = 0)
    val rawValueTransaction5 = prepareRawTx(firstAcc, toAccount = Some(secondAccount), value = Some(value2), nonce = 0)


    val transfer3 = service.ethSendRawTransaction(rawValueTransaction3).send()
    transfer3.getError shouldEqual null

    val transfer4 = service.ethSendRawTransaction(rawValueTransaction4).send()
    transfer4.getError shouldEqual null
    val t4hash = transfer4.getTransactionHash

    val transfer5 = service.ethSendRawTransaction(rawValueTransaction5).send()
    transfer5.getError shouldEqual null
    val t5hash = transfer5.getTransactionHash

    val mineBlock = service.blockObservable(false).toBlocking.first()

    mineBlock.getBlock.getTransactions.size() shouldEqual 2

    val minedhashes = mineBlock.getBlock.getTransactions.asScala.map(result => result.asInstanceOf[TransactionHash].get()).toList
    minedhashes should contain theSameElementsAs List(t4hash, t5hash)
  }

  it should "eth_sendTransaction for contract creation" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val latestNonce = response.getTransactionCount

    val response1 = service.ethSendTransaction(sampleTransaction).send()
    response1.getError shouldEqual null
    val hash = response1.getTransactionHash
    hash should not equal null


    // Wait for the transaction to be mined
    val minedTransaction = service.transactionObservable().toBlocking.first()

    minedTransaction.getHash shouldEqual hash
    minedTransaction.getGas.asBigInt shouldEqual defaultGas
    minedTransaction.getGasPrice.asBigInt shouldEqual defaultGasPrice
    minedTransaction.getValue.asBigInt shouldEqual BigInt(0)
    minedTransaction.getNonce.asBigInt shouldEqual latestNonce.asBigInt

    val response2 = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val latestNonce2 = response2.getTransactionCount
    latestNonce2.asBigInt shouldEqual latestNonce.add(1).asBigInt
  }

  it should "eth_getStorageAt and eth_getCode" taggedAs(PrivNet) in new ScenarioSetup {
    // eth_getStorageAt
    val response = service.ethSendTransaction(createContract(firstAccount.address, storageContract)).send()
    response.getTransactionHash should not equal null

    // Wait till transaction is mined
    val minedTransaction = service.transactionObservable().toBlocking.first()

    val response1 = service.ethGetTransactionReceipt(response.getTransactionHash).send()
    response1.getTransactionReceipt.isPresent shouldEqual true
    val receipt = response1.getTransactionReceipt.get()

    val contractAddress = receipt.getContractAddress

    val response2 = service.ethGetStorageAt(contractAddress, BigInt(0), latestBlock).send()

    hexToBigInt(response2.getData) shouldEqual pos0

    val response3 = service.ethGetStorageAt(contractAddress, shaPos, latestBlock).send()
    hexToBigInt(response3.getData) shouldEqual mapResult

    val response4 = service.ethGetStorageAt(secondAccount.address, BigInt(0), latestBlock).send()
    hexToBigInt(response4.getData) shouldEqual BigInt(0)

    val response5 = service.ethGetStorageAt(unexistingAccount.address, BigInt(0), latestBlock).send()
    hexToBigInt(response5.getData) shouldEqual BigInt(0)


    // eth_getCode
    val response6 = service.ethGetCode(contractAddress, latestBlock).send()
    response6.getCode shouldEqual StorageCodeRuntimeRepresentation

    val response7 = service.ethGetCode(secondAccount.address, latestBlock).send()
    response7.getCode shouldEqual emptyResponse

    val response8 = service.ethGetCode(unexistingAccount.address, latestBlock).send()
    response8.getCode shouldEqual emptyResponse
  }

  it should "eth_getTransactionByHash" taggedAs(PrivNet) in new ScenarioSetup {
    val response1 = service.ethSendTransaction(sampleTransaction).send()
    response1.getTransactionHash should not equal null
    val tHash = response1.getTransactionHash

    val response2 = service.ethGetTransactionByHash(tHash).send()
    response2.getTransaction.isPresent shouldEqual true

    // Wait for the transaction to be mined
    val minedTransaction = service.transactionObservable().toBlocking.first()

    val response3 = service.ethGetTransactionByHash(tHash).send()
    response3.getTransaction.isPresent shouldEqual true
  }

  it should "eth_getTransactionReceipt" taggedAs(PrivNet) in new ScenarioSetup {
    val response1 = service.ethSendTransaction(sampleTransaction).send()
    response1.getTransactionHash should not equal null
    val tHash = response1.getTransactionHash

    val response2 = service.ethGetTransactionReceipt(tHash).send()
    response2.getTransactionReceipt.isPresent shouldEqual false

    // Wait for the transaction to be mined
    val minedTransaction = service.transactionObservable().toBlocking.first()

    val response3 = service.ethGetTransactionByHash(tHash).send()
    response3.getTransaction.isPresent shouldEqual true
  }

  it should "eth_Call" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.ethCall(valueTransfer(firstAccount.address, secondAccount.address, 100), latestBlock).send()
    response.getValue shouldEqual emptyResponse

    val response2 = service.ethSendTransaction(createContract(firstAccount.address, counterContract)).send()
    val txHash = response2.getTransactionHash

    val minedTransaction = service.transactionObservable().toBlocking.first()


    val response3 = service.ethGetTransactionReceipt(txHash).send()
    response3.getTransactionReceipt.isPresent shouldBe true
    val receipt = response3.getTransactionReceipt.get()

    val response4 = service.ethCall(contractCall(firstAccount.address, receipt.getContractAddress, readCounterContract), latestBlock).send()
    hexToBigInt(response4.getValue) shouldEqual 0

    val response5 = service.ethSendTransaction(contractCall(firstAccount.address, receipt.getContractAddress, writeCounterContract(1))).send()
    val minedTransaction1 = service.transactionObservable().toBlocking.first()


    val response6 = service.ethCall(contractCall(firstAccount.address, receipt.getContractAddress, readCounterContract), latestBlock).send()

    hexToBigInt(response6.getValue) shouldEqual 1

    val response7 = service.ethSendTransaction(contractCall(firstAccount.address, receipt.getContractAddress, writeCounterContract(2))).send()
    response7.getError shouldEqual null

    val minedTransaction2 = service.transactionObservable().toBlocking.first()

    val response8 = service.ethCall(contractCall(firstAccount.address, receipt.getContractAddress, readCounterContract), pendingBlock).send()
    hexToBigInt(response8.getValue) shouldEqual 2


    val response9 = service.ethCall(contractCall(firstAccount.address, receipt.getContractAddress, readCounterContract, Some(10)), latestBlock).send()
    response9.getValue shouldEqual emptyResponse
  }

  it should "eth_estimateGas" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.ethEstimateGas(valueTransfer(firstAccount.address, secondAccount.address, 100)).send()
    response.getAmountUsed.asBigInt shouldEqual 21000

    // Currently when sending transaction where amount > sender.balance estimate_gas
    // defaults to highest possible value (block gas limit) but according to spreadsheet should default to 0
    // It is worth investigating - EC536
    val blockResponse = service.ethGetBlockByNumber(latestBlock, false).send()
    val response6 = service.ethEstimateGas(valueTransfer(firstAccount.address, secondAccount.address, firstAccount.balance + 500)).send()
    response6.getAmountUsed.asBigInt shouldEqual blockResponse.getBlock.getGasLimit.asBigInt

    val response1 = service.ethEstimateGas(createContract(firstAccount.address, testContract)).send()
    val gasEstimated = response1.getAmountUsed.asBigInt

    val response2 = service.ethSendTransaction(createContract(firstAccount.address, testContract)).send()
    val txHash = response2.getTransactionHash
    val minedTransaction = service.transactionObservable().toBlocking.first()

    val response3 = service.ethGetTransactionReceipt(txHash).send()
    response3.getTransactionReceipt.isPresent shouldEqual true
    val receipt = response3.getTransactionReceipt.get()

    receipt.getGasUsed.asBigInt shouldEqual gasEstimated


    val response4 = service.ethSendTransaction(createContract(firstAccount.address, testContract, Some((gasEstimated - 1).bigInteger))).send()
    val txHash1 = response4.getTransactionHash
    val minedTransaction1 = service.transactionObservable().toBlocking.first()

    val response5 = service.ethGetTransactionReceipt(txHash1).send()
    response5.getTransactionReceipt.isPresent shouldEqual true
    val receipt1 = response5.getTransactionReceipt.get()



  }

  it should "eth_getLogs" taggedAs(PrivNet) in new ScenarioSetup {
    val filter = new EthFilter()
    val transferAmount = 100
    val response1 = service.ethSendTransaction(valueTransfer(firstAccount.address, secondAccount.address, transferAmount)).send()
    response1.getError shouldEqual null

    val response = service.ethGetLogs(filter).send()

    val logs = response.getLogs

    1 shouldEqual 1
  }

  it should "personal_personalListAccounts" taggedAs(PrivNet) in new ScenarioSetup {
    val response = service.personalListAccounts().send()
    val accounts = response.getAccountIds.asScala

    accounts should contain allOf (firstAccount.address, secondAccount.address, thirdAccount.address, gethAccount.address)
  }

  // There is no api to delete account, so to avoid creating a lot of unnecessery key stores several methods in one test
  it should "personal_newAccount and personal_unlockAccount and personal_sendTransaction" taggedAs(PrivNet) in new ScenarioSetup {
    val acc1Pass = "1234567"
    val response = service.personalNewAccount(acc1Pass).send()
    val newAccId = response.getAccountId

    val response1 = service.personalNewAccount("asdjhf lkljds").send()
    val newAccId1 = response1.getAccountId

    val response2 = service.personalListAccounts().send()
    val accounts = response2.getAccountIds.asScala
    accounts should contain allOf (newAccId, newAccId1)


    val transfer1 = service.ethSendTransaction(valueTransfer(firstAccount.address, newAccId, 1000)).send()

    val mined = service.transactionObservable().toBlocking.first()

    val transfer2 = service.personalSendTransaction(valueTransfer(newAccId, newAccId1, 100), "badpass").send()
    transfer2.getError should not equal null
    transfer2.getError.getMessage shouldEqual "Could not decrypt key with given passphrase"

    val unlock = service.personalUnlockAccount(newAccId, "badpass", 0).send()
    unlock.getError should not equal null
    unlock.getError.getMessage shouldEqual "Could not decrypt key with given passphrase"

    val transfer3 = service.personalSendTransaction(valueTransfer(newAccId, newAccId1, 100), acc1Pass).send()
    transfer3.getError shouldEqual null
    transfer3.getTransactionHash should not equal null

    val unlock1 = service.personalUnlockAccount(newAccId, acc1Pass, 5).send()
    unlock1.accountUnlocked() shouldEqual true
    unlock1.getError shouldEqual null

    val transfer4 = service.ethSendTransaction(valueTransfer(newAccId, firstAccount.address, 10)).send()
    transfer4.getError shouldEqual null

    // Wait 5s
    Thread.sleep(5000)

    val transfer5 = service.ethSendTransaction(valueTransfer(newAccId, firstAccount.address, 10)).send()
    transfer5.getError should not equal null
  }


}

trait ScenarioSetup {
  val service = Admin.build(new HttpService("http://localhost:8546/"))
  val unexisitingBlockHash = "0xaaaaaaaaaaa959b3db6469104c59b803162cf37a23293e8df306e559218f5c6f"
  val badHash = "0xm"
  val emptyResponse = "0x"

  val futureBlock = DefaultBlockParameter.valueOf(BigInt(50000000000000L).bigInteger)
  val latestBlock = DefaultBlockParameter.valueOf("latest")
  val pendingBlock = DefaultBlockParameter.valueOf("pending")
  val earliestBlock = DefaultBlockParameter.valueOf("earliest")

  def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    val normalized = if (stripped.length % 2 == 1) "0" + stripped else stripped
    Hex.decode(normalized)
  }

  def hexToBigInt(s: String): BigInt = {
    BigInt(decode(s))
  }

  implicit class BigIntegerExt(val x: BigInteger) {
    def asBigInt: BigInt = BigInt(x)
  }
  implicit def intToBigInt(x: Int): BigInteger = BigInt(x).bigInteger
  implicit def BigIntToBingInteger(x: BigInt): BigInteger = x.bigInteger

  case class TestBlock(hash: String, number: BigInt, transactions: List[TestTransaction] = List.empty , uncles: List[TestUncle] = List.empty) {
    lazy val blockNumber = DefaultBlockParameter.valueOf(number.bigInteger)
  }
  case class TestTransaction(hash: String, index: BigInt)
  case class TestUncle(hash: String, index: BigInt)
  case class TestAccount(address: String, password: String, balance: BigInt)

  // MainNet existing blocks
  val oneUncleTestBlock = TestBlock("0xb055593f14d994d333d4a7fbf6251c8fef00d24ca74e1355de90729ca9b6d7f5", 1024,
    uncles = List(TestUncle("0xe00ecf0381819d0b8cf86eed3e1d91db26b3e78f80bab8b388768a49210c7e6f", 0))
  )
  val noTransactionsOrUnclesBlock = TestBlock("0x73b20034e531f385a59401bbda9a225be12b2fd42d7c21e4c3d11b3d7be34244", 2000)
  val twoTransactionBlock = TestBlock("0x3152ec08cab959b3db6469104c59b803162cf37a23293e8df306e559218f5c6f", 127117,
    List(
      TestTransaction("0x1f76a07cc698c1ef425c406ede5db8fa5ed67f084397ac74fd48348bd08fbb1d", 0),
      TestTransaction("0x4fcf5556f2ebb8d47e1335641fa1b1f1fd24e41645f666fa8b16af6c03eddf19", 1)
    )
  )

  // Exisiting privatenet Accounts
  val firstAccount = TestAccount("0x316158e265fa708c623cc3094b2bb5889e0f5ca5", "hunter2", BigInt("100000000000000000000"))
  val secondAccount = TestAccount("0xb9ec69316a8810db91c36de79c4f1e785f2c35fc", "", BigInt("100000000000000000000"))

  // Account not used in value transfers
  val thirdAccount = TestAccount("0x488c10c91771d3b3c25f63b6414309b119baacb5", "", BigInt("100000000000000000000"))

  val gethAccount = TestAccount("0x03010360ebbb7f49362a2c650a67661d464e2089", "", BigInt("0"))

  val unexistingAccount = TestAccount("0xaaaa10c91771d3b3c25f63b6414309b119baacb5", "", BigInt("100000000000000000000"))

  //https://github.com/rsksmart/rskj/wiki/Deploying-contracts-using-RPC-calls#publishing-a-contract-using-rpc
  val testContract = "6060604052341561000c57fe5b5b6101598061001c6000396000f30060606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063cfae32171461003b575bfe5b341561004357fe5b61004b6100d4565b604051808060200182810382528381815181526020019150805190602001908083836000831461009a575b80518252602083111561009a57602082019150602081019050602083039250610076565b505050905090810190601f1680156100c65780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b6100dc610119565b604060405190810160405280600381526020017f486921000000000000000000000000000000000000000000000000000000000081525090505b90565b6020604051908101604052806000815250905600a165627a7a72305820ed71008611bb64338581c5758f96e31ac3b0c57e1d8de028b72f0b8173ff93a10029"

  import io.iohk.ethereum.crypto.kec256

  // https://github.com/ethereum/wiki/wiki/JSON-RPC#example-14
  val storageContract = "0x60606040525b6104d260006000508190555061162e600160005060003373ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050819055505b600a8060546000396000f360606040526008565b00"
  val pos0 = BigInt(1234)
  val mapPos = "000000000000000000000000" + firstAccount.address.drop(2) + "0000000000000000000000000000000000000000000000000000000000000001"
  val decoded = Hex.decode(mapPos)
  val shaPos = BigInt(kec256(decoded))
  val mapResult  = BigInt(5678)
  val StorageCodeRuntimeRepresentation = "0x60606040526008565b00"



  /*
  *
  * contract Test {
      int256 pos0 = 0;

      function double(int256 a) {
          pos0 = a;
      }

      function read() returns(int) {
          return pos0;
      }
    }
  *
  *
  *
  * */
  val counterContract = "0x6060604052600060006000505560978060186000396000f360606040526000357c01000000000000000000000000000000000000000000000000000000009004806357de26a41460415780636ffa1caa14606257603f565b005b604c60048050506086565b6040518082815260200191505060405180910390f35b607660048080359060200190919050506078565b005b806000600050819055505b50565b600060006000505490506094565b9056"
  val readCounterContract = "57de26a4" + "0000000000000000000000000000000000000000000000000000000000000000"
  //val writeCounterContract = "6ffa1caa" + "0000000000000000000000000000000000000000000000000000000000000001"
  def writeCounterContract(a: BigInt): String = {
    val funName = "6ffa1caa"
    import io.iohk.ethereum.utils.ByteUtils
    val asByteString = ByteString(a.toByteArray)
    funName + Hex.toHexString(ByteUtils.padLeft(asByteString, 32).toArray)
  }




  val defaultGasPrice = BigInt(20000000000L)
  val defaultGas = BigInt(90000)
  val sampleTransaction = new Transaction (
    firstAccount.address,
    null,
    null,
    null,
    null,
    null,
    testContract
  )
  val coinbase = "0x0011223344556677889900112233445566778899"

  def createContract(address: String, code: String, gasLimit: Option[BigInteger] = None): Transaction = {
    new Transaction (
      address,
      null,
      null,
      gasLimit.orNull,
      null,
      null,
      code
    )
  }

  def contractCall(address: String, to: String, data: String, gasLimit: Option[BigInteger] = None): Transaction = {
    new Transaction (
      address,
      null,
      null,
      gasLimit.orNull,
      to,
      null,
      data
    )
  }

  def valueTransfer(from: String, to: String, amount: BigInt, nonce: Option[BigInteger] = None, gasLimit: Option[BigInteger] = None): Transaction = {
    new Transaction (
      from,
      nonce.orNull,
      null,
      gasLimit.orNull,
      to,
      amount,
      null
    )
  }

  def setupTwoNewAccounts(fundsProvider: String, amount: BigInt): (TestAccount, TestAccount) = {
    val first = service.personalNewAccount("").send().getAccountId
    val second = service.personalNewAccount("").send().getAccountId

    val firstUnlock = service.personalUnlockAccount(first, "", 0).send()
    val secondUnlock = service.personalUnlockAccount(second, "", 0).send()

    val trans = service.ethSendTransaction(valueTransfer(fundsProvider, first, amount)).send()
    val trans1 = service.ethSendTransaction(valueTransfer(fundsProvider, second, amount)).send()

    // wait for mine
    val block = service.blockObservable(false).toBlocking().first()

    (TestAccount(first, "", amount), TestAccount(second, "", amount))
  }

  val home = System.getProperty("user.home")
  val keyStoreConfig = KeyStoreConfig.customKeyStoreConfig(s"${home}/.privateNetTest/keystore")
  val keyStore = new KeyStoreImpl(keyStoreConfig, new SecureRandom())

  def getAccountWallet(address: String, pass: String): Wallet = {
    keyStore.unlockAccount(Address(address), pass) match {
      case Right(w) => w
      case Left(err) => throw new RuntimeException(s"Cannot get wallet, because of $err")
    }
  }

  def prepareRawTx(
    fromAccount: TestAccount,
    toAccount: Option[TestAccount] = None,
    value: Option[BigInt] = None,
    data: Option[ByteString] = None,
    nonce: BigInt): String = {
    val fromAddress = Address(fromAccount.address)
    val fromWallet = getAccountWallet(fromAccount.address, fromAccount.password)

    val req = TransactionRequest(
      fromAddress,
      to = toAccount.map(acc => Address(acc.address)),
      value = value,
      data = data,
      nonce = Some(nonce))

    val transaction = req.toTransaction(0)

    val stx = fromWallet.signTx(transaction, None)
    Hex.toHexString(rlp.encode(stx.toRLPEncodable))
  }

}