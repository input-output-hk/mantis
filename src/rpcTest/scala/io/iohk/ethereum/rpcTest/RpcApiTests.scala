package io.iohk.ethereum.rpcTest

import java.math.BigInteger
import java.security.SecureRandom

import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.TransactionRequest
import io.iohk.ethereum.keystore.{KeyStoreImpl, Wallet}
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rpcTest.Tags.{MainNet, PrivNet, PrivNetNoMining}
import io.iohk.ethereum.utils.{KeyStoreConfig, Logger}
import org.bouncycastle.util.encoders.Hex
import org.web3j.protocol.admin.Admin
import org.web3j.protocol.core.DefaultBlockParameter
import org.web3j.protocol.core.methods.request.{EthFilter, Transaction}
import org.web3j.protocol.core.methods.response.EthBlock.{TransactionHash, TransactionObject}
import org.web3j.protocol.http.HttpService
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import org.web3j.protocol.core.methods.response.EthLog.{Hash, LogObject}
import io.iohk.ethereum.rpcTest.TestContracts._
import io.iohk.ethereum.rpcTest.TestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.web3j.protocol.core.methods.response.EthLog

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class RpcApiTests extends AnyFlatSpec with Matchers with Logger {

  ("Json rpc service" should "be listening before all tests").taggedAs(MainNet, PrivNet) in new ScenarioSetup {
    val response = service.netListening().send()
    response.isListening shouldBe true
  }

  (it should "return correct protocol version").taggedAs(MainNet, PrivNet) in new ScenarioSetup {
    val response = service.ethProtocolVersion().send()
    hexToBigInt(response.getProtocolVersion) shouldEqual protocolVersion
  }

  (it should "return correct client version").taggedAs(MainNet, PrivNet) in new ScenarioSetup {
    val response = service.web3ClientVersion().send()
    response.getWeb3ClientVersion shouldEqual clientVersion
  }

  (it should "correctly calculate Sha3").taggedAs(MainNet, PrivNet) in new ScenarioSetup {
    val response = service.web3Sha3("").send()
    response.getResult shouldEqual "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
  }

  it should "eth_getBlockTransactionCountByHash" taggedAs (MainNet) in new ScenarioSetup {
    val response = service.ethGetBlockTransactionCountByHash(noTransactionsOrUnclesBlock.hash).send()
    response.getTransactionCount.asBigInt shouldEqual noTransactionsOrUnclesBlock.transactions.size

    val response1 = service.ethGetBlockTransactionCountByHash(twoTransactionBlock.hash).send()
    response1.getTransactionCount.asBigInt shouldEqual twoTransactionBlock.transactions.size

    val response2 = service.ethGetBlockTransactionCountByHash(unexisitingBlockHash).send()
    response2.getResult shouldBe null
  }

  it should "eth_getBlockTransactionCountByNumber" taggedAs (MainNet) in new ScenarioSetup {
    val response = service.ethGetBlockTransactionCountByNumber(noTransactionsOrUnclesBlock.blockNumber).send()
    response.getTransactionCount.asBigInt shouldEqual noTransactionsOrUnclesBlock.transactions.size

    val response1 = service.ethGetBlockTransactionCountByNumber(twoTransactionBlock.blockNumber).send()
    response1.getTransactionCount.asBigInt shouldEqual twoTransactionBlock.transactions.size

    val response2 = service.ethGetBlockTransactionCountByNumber(futureBlock).send()
    response2.getResult shouldBe null
    response2.getError.getCode shouldEqual generalErrorCode
  }

  it should "eth_getUncleCountByBlockHash" taggedAs (MainNet) in new ScenarioSetup {
    val response = service.ethGetUncleCountByBlockHash(noTransactionsOrUnclesBlock.hash).send()
    response.getUncleCount.asBigInt shouldEqual noTransactionsOrUnclesBlock.uncles.size

    val response1 = service.ethGetUncleCountByBlockHash(oneUncleTestBlock.hash).send()
    response1.getUncleCount.asBigInt shouldEqual oneUncleTestBlock.uncles.size

    val response2 = service.ethGetUncleCountByBlockHash(unexisitingBlockHash).send()
    response2.getResult shouldBe null
    response2.getError.getCode shouldEqual generalErrorCode
  }

  it should "eth_getUncleCountByBlockNumber" taggedAs (MainNet) in new ScenarioSetup {
    val response = service.ethGetUncleCountByBlockNumber(noTransactionsOrUnclesBlock.blockNumber).send()
    response.getUncleCount.asBigInt shouldEqual noTransactionsOrUnclesBlock.uncles.size

    val response1 = service.ethGetUncleCountByBlockNumber(oneUncleTestBlock.blockNumber).send()
    response1.getUncleCount.asBigInt shouldEqual oneUncleTestBlock.uncles.size

    val response2 = service.ethGetUncleCountByBlockNumber(futureBlock).send()
    response2.getResult shouldBe null
    response2.getError.getCode shouldEqual generalErrorCode
  }

  it should "eth_getBlockByHash" taggedAs (MainNet) in new ScenarioSetup {
    val response = service.ethGetBlockByHash(twoTransactionBlock.hash, false).send()
    response.getBlock.getNumber.asBigInt shouldEqual twoTransactionBlock.number

    val response1 = service.ethGetBlockByHash(unexisitingBlockHash, false).send()
    response1.getBlock shouldEqual null
    response1.getError shouldEqual null

    val response2 = service.ethGetBlockByHash(badHash, false).send()
    response2.getBlock shouldEqual null
    response2.getError.getCode shouldEqual generalErrorCode
  }

  it should "eth_getTransactionByBlockHashAndIndex" taggedAs (MainNet) in new ScenarioSetup {
    val response = service
      .ethGetTransactionByBlockHashAndIndex(twoTransactionBlock.hash, twoTransactionBlock.transactions.head.index)
      .send()
    response.getTransaction.isPresent shouldBe true
    response.getTransaction.get().getHash shouldEqual twoTransactionBlock.transactions.head.hash

    val response1 = service.ethGetTransactionByBlockHashAndIndex(unexisitingBlockHash, 0).send()
    response1.getTransaction.isPresent shouldEqual false
    response1.getError shouldEqual null

    val response2 = service
      .ethGetTransactionByBlockHashAndIndex(twoTransactionBlock.hash, twoTransactionBlock.transactions.size + 1)
      .send()
    response2.getTransaction.isPresent shouldEqual false
    response2.getError shouldEqual null
  }

  it should "eth_getTransactionByBlockNumberAndIndex" taggedAs (MainNet) in new ScenarioSetup {
    val response = service
      .ethGetTransactionByBlockNumberAndIndex(
        twoTransactionBlock.blockNumber,
        twoTransactionBlock.transactions.head.index
      )
      .send()
    response.getTransaction.isPresent shouldBe true
    response.getTransaction.get().getHash shouldEqual twoTransactionBlock.transactions.head.hash

    val response1 = service.ethGetTransactionByBlockNumberAndIndex(futureBlock, 0).send()
    response1.getTransaction.isPresent shouldEqual false
    response1.getError shouldEqual null

    val response2 = service
      .ethGetTransactionByBlockNumberAndIndex(
        twoTransactionBlock.blockNumber,
        twoTransactionBlock.transactions.size + 1
      )
      .send()
    response2.getTransaction.isPresent shouldEqual false
    response2.getError shouldEqual null
  }

  it should "eth_getUncleByBlockHashAndIndex" taggedAs (MainNet) in new ScenarioSetup {
    val response =
      service.ethGetUncleByBlockHashAndIndex(oneUncleTestBlock.hash, oneUncleTestBlock.uncles.head.index).send()
    response.getBlock.getHash shouldEqual oneUncleTestBlock.uncles.head.hash

    val response1 = service.ethGetUncleByBlockHashAndIndex(unexisitingBlockHash, 0).send()
    response1.getBlock shouldEqual null
    response1.getError shouldEqual null

    val response2 =
      service.ethGetUncleByBlockHashAndIndex(oneUncleTestBlock.hash, oneUncleTestBlock.uncles.head.index + 1).send()
    response2.getBlock shouldEqual null
    response2.getError shouldEqual null
  }

  it should "eth_getBlockByNumber without mining" taggedAs (MainNet) in new ScenarioSetup {
    val response = service.ethGetBlockByNumber(latestBlock, false).send()
    val lBlock = response.getBlock
    lBlock should not equal null

    val response1 = service.ethGetBlockByNumber(pendingBlock, false).send()
    val pBlock = response1.getBlock
    pBlock should not equal null

    pBlock.getNumber should be >= lBlock.getNumber
  }

  it should "eth_getUncleByBlockNumberAndIndex" taggedAs (MainNet) in new ScenarioSetup {
    val response = service
      .ethGetUncleByBlockNumberAndIndex(oneUncleTestBlock.blockNumber, oneUncleTestBlock.uncles.head.index)
      .send()
    response.getBlock.getHash shouldEqual oneUncleTestBlock.uncles.head.hash

    val response1 = service.ethGetUncleByBlockNumberAndIndex(futureBlock, 0).send()
    response1.getBlock shouldEqual null
    response1.getError shouldEqual null

    val response2 = service
      .ethGetUncleByBlockNumberAndIndex(oneUncleTestBlock.blockNumber, oneUncleTestBlock.uncles.head.index + 1)
      .send()
    response2.getBlock shouldEqual null
    response2.getError shouldEqual null
  }

  it should "eth_mining false on MainNet" taggedAs (MainNet) in new ScenarioSetup {
    val response = service.ethMining().send()
    response.isMining shouldEqual false
  }

  it should "eth_hashrate equal to 0 on MainNet" taggedAs (MainNet) in new ScenarioSetup {
    val response = service.ethHashrate().send()
    response.getHashrate.asBigInt shouldEqual 0
  }

  it should "eth_getBalance" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethGetBalance(thirdAccount.address, latestBlock).send()
    response.getBalance.asBigInt shouldEqual thirdAccount.balance

    val response1 = service.ethGetBalance(unexistingAccount.address, latestBlock).send()
    response1.getBalance.asBigInt shouldEqual 0
  }

  it should "eth_BlockNumber" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethBlockNumber().send()
    response.getBlockNumber should not equal null
    val receivedNumber = response.getBlockNumber.asBigInt

    // Wait for new block
    val minedBlock1 = service.blockFlowable(false).blockingFirst()

    val response1 = service.ethBlockNumber().send()
    response1.getBlockNumber should not equal null
    response1.getBlockNumber.asBigInt should be > receivedNumber
  }

  it should "eth_mining true on privNet" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethMining().send()
    response.isMining shouldEqual true
  }

  it should "eth_coinbase on privNet" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethCoinbase().send()
    response.getAddress shouldEqual coinbase
  }

  it should "eth_hashrate larger than 0 on privNet" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethHashrate().send()
    response.getHashrate.asBigInt should not equal 0
  }

  it should "personal_unlockAccount for indefinite duration" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.personalUnlockAccount(firstAccount.address, firstAccount.password, BigInt(0)).send()
    response.accountUnlocked() shouldEqual true
  }

  it should "eth_getBlockByNumber with mining" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethGetBlockByNumber(latestBlock, false).send()
    val lBlock = response.getBlock
    lBlock should not equal null

    val response3 = service.ethGetBlockByNumber(pendingBlock, false).send()
    val pBlock = response3.getBlock
    pBlock should not equal null

    val response1 = service.ethGetBlockByNumber(getBlockParam(lBlock.getNumber), false).send()
    val nBlock = response1.getBlock
    nBlock should not equal null

    lBlock.getHash shouldEqual nBlock.getHash

    val response2 = service.ethGetBlockByNumber(earliestBlock, false).send()
    val eBlock = response2.getBlock
    eBlock should not equal null

    eBlock.getNumber.asBigInt shouldEqual 0

    val response4 = service.ethSendTransaction(sampleTransaction).send()
    response4.getTransactionHash should not equal null
    val tHash = response4.getTransactionHash

    // Wait for the transaction to be mined
    val minedTransaction = service.transactionFlowable().blockingFirst()
    minedTransaction.getHash shouldEqual tHash

    val response5 = service.ethGetBlockByNumber(getBlockParam(minedTransaction.getBlockNumber), false).send()
    val transaction1 = response5.getBlock.getTransactions.get(0)
    transaction1 shouldBe a[TransactionHash]
    transaction1.asInstanceOf[TransactionHash].get() shouldEqual minedTransaction.getHash

    val response6 = service.ethGetBlockByNumber(getBlockParam(minedTransaction.getBlockNumber), true).send()
    val transaction2 = response6.getBlock.getTransactions.get(0)
    transaction2 shouldBe a[TransactionObject]
    transaction2.asInstanceOf[TransactionObject].get().getHash shouldEqual minedTransaction.getHash
    transaction2.asInstanceOf[TransactionObject].get().getFrom shouldEqual minedTransaction.getFrom

    val response7 = service.ethGetBlockByNumber(futureBlock, false).send()
    response7.getBlock shouldEqual null
  }

  it should "eth_getTransactionCount with mining" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val currentCount = response.getTransactionCount.asBigInt

    val transferAmount = 100
    val response1 =
      service.ethSendTransaction(valueTransfer(firstAccount.address, secondAccount.address, transferAmount)).send()
    response1.getError shouldEqual null

    val minedTransactions = service.transactionFlowable().blockingFirst()

    val response2 = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val currentCount2 = response2.getTransactionCount.asBigInt

    currentCount2 shouldEqual currentCount + 1

    val response3 = service.ethGetTransactionCount(unexistingAccount.address, latestBlock).send()
    val currentCount3 = response3.getTransactionCount.asBigInt

    currentCount3 shouldEqual 0
  }

  it should "not send eth_sendTransaction from locked account" taggedAs (PrivNet) in new ScenarioSetup {
    val response1 = service.ethSendTransaction(createContract(thirdAccount.address, testContract)).send()

    response1.getTransactionHash shouldEqual null
    response1.getError should not equal null
    response1.getError.getMessage shouldEqual "account is locked or unknown"
  }

  it should "eth_sendTransaction for value transfer" taggedAs (PrivNet) in new ScenarioSetup {
    val transferAmaunt = 100
    val firstAccountstartBalance = service.ethGetBalance(firstAccount.address, latestBlock).send().getBalance.asBigInt
    val secondAccountstartBalance = service.ethGetBalance(secondAccount.address, latestBlock).send().getBalance.asBigInt

    val response1 =
      service.ethSendTransaction(valueTransfer(firstAccount.address, secondAccount.address, transferAmaunt)).send()
    response1.getError shouldEqual null

    val mined = service.transactionFlowable().blockingFirst()
    val receipt = service.ethGetTransactionReceipt(response1.getTransactionHash).send().getTransactionReceipt
    receipt.isPresent shouldEqual true

    val transactionCost = receipt.get().getCumulativeGasUsed.asBigInt * mined.getGasPrice.asBigInt

    val firstAccountEndBalance = service.ethGetBalance(firstAccount.address, latestBlock).send().getBalance.asBigInt
    val secondAccountEndBalance = service.ethGetBalance(secondAccount.address, latestBlock).send().getBalance.asBigInt

    firstAccountstartBalance - transferAmaunt - transactionCost shouldEqual firstAccountEndBalance
    secondAccountstartBalance + transferAmaunt shouldEqual secondAccountEndBalance
  }

  it should "eth_sendTransaction with several transactions in pool with same nonce" taggedAs (PrivNet) in new ScenarioSetup {
    val fundingAmount = 20000000000000000L
    val amount1 = 100
    val amount2 = 200
    val amount3 = 300
    val response = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val txCount = response.getTransactionCount.asBigInt

    val transfer = service
      .ethSendTransaction(valueTransfer(firstAccount.address, secondAccount.address, amount1, Some(txCount)))
      .send()
    transfer.getError shouldEqual null

    val transfer1 = service
      .ethSendTransaction(valueTransfer(firstAccount.address, secondAccount.address, amount2, Some(txCount)))
      .send()
    transfer1.getError shouldEqual null

    val mined = service.transactionFlowable().blockingFirst()

    mined.getValue.asBigInt shouldEqual amount2

    val response2 = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    response2.getTransactionCount.asBigInt shouldEqual txCount + 1

    // Transactions from two accounts with same nonce
    val (firstAcc, secondAcc) = setupTwoNewAccounts(firstAccount.address, fundingAmount)

    val transfer3 =
      service.ethSendTransaction(valueTransfer(firstAcc.address, secondAccount.address, amount1, Some(0))).send()
    transfer3.getError shouldEqual null

    val transfer4 =
      service.ethSendTransaction(valueTransfer(secondAcc.address, secondAccount.address, amount2, Some(0))).send()
    transfer4.getError shouldEqual null
    val t4hash = transfer4.getTransactionHash

    val transfer5 =
      service.ethSendTransaction(valueTransfer(firstAcc.address, secondAccount.address, amount3, Some(0))).send()
    transfer5.getError shouldEqual null
    val t5hash = transfer5.getTransactionHash

    val mineBlock = service
      .blockFlowable(false)
      .filter(block => block.getBlock.getTransactions.size == 2)
      .blockingFirst()

    val minedhashes =
      mineBlock.getBlock.getTransactions.asScala.map(result => result.asInstanceOf[TransactionHash].get()).toList
    minedhashes should contain theSameElementsAs List(t4hash, t5hash)
  }

  it should "eth_sendRawTransaction" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val nextNonce = response.getTransactionCount.asBigInt

    val value = 100
    val rawValueTransaction =
      prepareRawTx(firstAccount, toAccount = Some(secondAccount), value = Some(value), nonce = nextNonce)
    val response1 = service.ethSendRawTransaction(rawValueTransaction).send
    val txHash = response1.getTransactionHash

    val minedTransaction = service.transactionFlowable().blockingFirst()
    minedTransaction.getHash shouldEqual txHash

    val contractCreation =
      prepareRawTx(firstAccount, data = Some(ByteString(decode(storageContract))), nonce = nextNonce + 1)
    val response2 = service.ethSendRawTransaction(contractCreation).send
    val txHash1 = response2.getTransactionHash

    val minedTransaction1 = service.transactionFlowable().blockingFirst()
    minedTransaction1.getHash shouldEqual txHash1

    val value1 = 200
    val value2 = 300

    // two transactions with same nonce, only second will get included in blockchain
    val rawValueTransaction1 =
      prepareRawTx(firstAccount, toAccount = Some(secondAccount), value = Some(value1), nonce = nextNonce + 2)
    val rawValueTransaction2 =
      prepareRawTx(firstAccount, toAccount = Some(secondAccount), value = Some(value2), nonce = nextNonce + 2)

    val response3 = service.ethSendRawTransaction(rawValueTransaction1).send()
    response3.getError shouldEqual null

    val response4 = service.ethSendRawTransaction(rawValueTransaction2).send()
    response4.getError shouldEqual null

    val minedTransaction2 = service.transactionFlowable().blockingFirst()

    minedTransaction2.getValue.asBigInt shouldEqual value2

    val fundingAmount = 20000000000000000L
    // Transactions from two accounts with same nonce
    val (firstAcc, secondAcc) = setupTwoNewAccounts(firstAccount.address, fundingAmount)

    val rawValueTransaction3 = prepareRawTx(firstAcc, toAccount = Some(secondAccount), value = Some(value), nonce = 0)
    val rawValueTransaction4 = prepareRawTx(secondAcc, toAccount = Some(secondAccount), value = Some(value1), nonce = 0)
    val rawValueTransaction5 = prepareRawTx(firstAcc, toAccount = Some(secondAccount), value = Some(value2), nonce = 0)

    val waitForNextBlock = service.blockFlowable(false).blockingFirst()

    val transfer3 = service.ethSendRawTransaction(rawValueTransaction3).send()
    transfer3.getError shouldEqual null

    val transfer4 = service.ethSendRawTransaction(rawValueTransaction4).send()
    transfer4.getError shouldEqual null
    val t4hash = transfer4.getTransactionHash

    val transfer5 = service.ethSendRawTransaction(rawValueTransaction5).send()
    transfer5.getError shouldEqual null
    val t5hash = transfer5.getTransactionHash

    val mineBlock = service
      .blockFlowable(false)
      .filter(block => block.getBlock.getTransactions.size() == 2)
      .blockingFirst()

    val minedhashes =
      mineBlock.getBlock.getTransactions.asScala.map(result => result.asInstanceOf[TransactionHash].get()).toList
    minedhashes should contain theSameElementsAs List(t4hash, t5hash)
  }

  it should "eth_sendTransaction for contract creation" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val latestNonce = response.getTransactionCount

    val response1 = service.ethSendTransaction(sampleTransaction).send()
    response1.getError shouldEqual null
    val hash = response1.getTransactionHash
    hash should not equal null
    // Wait for the transaction to be mined
    val minedTransaction = service.transactionFlowable().blockingFirst()

    minedTransaction.getHash shouldEqual hash
    minedTransaction.getGas.asBigInt shouldEqual defaultGas
    minedTransaction.getGasPrice.asBigInt shouldEqual defaultGasPrice
    minedTransaction.getValue.asBigInt shouldEqual 0
    minedTransaction.getNonce.asBigInt shouldEqual latestNonce.asBigInt

    val response2 = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val latestNonce2 = response2.getTransactionCount
    latestNonce2.asBigInt shouldEqual latestNonce.add(1).asBigInt
  }

  it should "eth_getStorageAt and eth_getCode" taggedAs (PrivNet) in new ScenarioSetup {
    // eth_getStorageAt
    val response = service.ethSendTransaction(createContract(firstAccount.address, storageContract)).send()
    response.getTransactionHash should not equal null

    // Wait till transaction is mined
    val minedTransaction = service.transactionFlowable().blockingFirst()

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

  it should "eth_getTransactionByHash" taggedAs (PrivNet) in new ScenarioSetup {
    val response1 = service.ethSendTransaction(sampleTransaction).send()
    response1.getTransactionHash should not equal null
    val tHash = response1.getTransactionHash

    val response2 = service.ethGetTransactionByHash(tHash).send()
    response2.getTransaction.isPresent shouldEqual true

    // Wait for the transaction to be mined
    val minedTransaction = service.transactionFlowable().blockingFirst()

    val response3 = service.ethGetTransactionByHash(tHash).send()
    response3.getTransaction.isPresent shouldEqual true
  }

  it should "eth_getTransactionReceipt" taggedAs (PrivNet) in new ScenarioSetup {
    val response1 = service.ethSendTransaction(sampleTransaction).send()
    response1.getTransactionHash should not equal null
    val tHash = response1.getTransactionHash

    val response2 = service.ethGetTransactionReceipt(tHash).send()
    response2.getTransactionReceipt.isPresent shouldEqual false

    // Wait for the transaction to be mined
    val minedTransaction = service.transactionFlowable().blockingFirst()

    val response3 = service.ethGetTransactionByHash(tHash).send()
    response3.getTransaction.isPresent shouldEqual true
  }

  it should "eth_Call" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethCall(valueTransfer(firstAccount.address, secondAccount.address, 100), latestBlock).send()
    response.getValue shouldEqual emptyResponse

    val response2 = service.ethSendTransaction(createContract(firstAccount.address, counterEventContract)).send()
    val txHash = response2.getTransactionHash

    val minedTransaction = service.transactionFlowable().blockingFirst()

    val response3 = service.ethGetTransactionReceipt(txHash).send()
    response3.getTransactionReceipt.isPresent shouldBe true
    val receipt = response3.getTransactionReceipt.get()

    val response4 = service
      .ethCall(contractCall(firstAccount.address, receipt.getContractAddress, readEventContract), latestBlock)
      .send()
    hexToBigInt(response4.getValue) shouldEqual 0

    val response5 = service
      .ethSendTransaction(
        contractCall(firstAccount.address, receipt.getContractAddress, writeContract(1, incrementEventContract))
      )
      .send()
    val minedTransaction1 = service.transactionFlowable().blockingFirst()

    val response6 = service
      .ethCall(contractCall(firstAccount.address, receipt.getContractAddress, readEventContract), latestBlock)
      .send()

    hexToBigInt(response6.getValue) shouldEqual 1

    val response7 = service
      .ethSendTransaction(
        contractCall(firstAccount.address, receipt.getContractAddress, writeContract(2, incrementEventContract))
      )
      .send()
    response7.getError shouldEqual null

    val minedTransaction2 = service.transactionFlowable().blockingFirst()

    val response8 = service
      .ethCall(contractCall(firstAccount.address, receipt.getContractAddress, readEventContract), pendingBlock)
      .send()
    hexToBigInt(response8.getValue) shouldEqual 2

    val response9 = service
      .ethCall(contractCall(firstAccount.address, receipt.getContractAddress, readEventContract, Some(10)), latestBlock)
      .send()
    response9.getValue shouldEqual emptyResponse
  }

  it should "eth_estimateGas" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.ethEstimateGas(valueTransfer(firstAccount.address, secondAccount.address, 100)).send()
    response.getAmountUsed.asBigInt shouldEqual 21000

    // Currently when sending transaction where amount > sender.balance estimate_gas
    // defaults to highest possible value (block gas limit) but according to spreadsheet should default to 0
    // It is worth investigating - EC536
    val blockResponse = service.ethGetBlockByNumber(latestBlock, false).send()
    val response6 = service
      .ethEstimateGas(valueTransfer(firstAccount.address, secondAccount.address, firstAccount.balance + 500))
      .send()
    response6.getAmountUsed.asBigInt shouldEqual blockResponse.getBlock.getGasLimit.asBigInt

    val response1 = service.ethEstimateGas(createContract(firstAccount.address, testContract)).send()
    val gasEstimated = response1.getAmountUsed.asBigInt

    val response2 = service.ethSendTransaction(createContract(firstAccount.address, testContract)).send()
    val txHash = response2.getTransactionHash
    val minedTransaction = service.transactionFlowable().blockingFirst()

    val response3 = service.ethGetTransactionReceipt(txHash).send()
    response3.getTransactionReceipt.isPresent shouldEqual true
    val receipt = response3.getTransactionReceipt.get()

    receipt.getGasUsed.asBigInt shouldEqual gasEstimated

    val response4 = service
      .ethSendTransaction(createContract(firstAccount.address, testContract, Some((gasEstimated - 1).bigInteger)))
      .send()
    val txHash1 = response4.getTransactionHash
    val minedTransaction1 = service.transactionFlowable().blockingFirst()

    val response5 = service.ethGetTransactionReceipt(txHash1).send()
    response5.getTransactionReceipt.isPresent shouldEqual true
    val receipt1 = response5.getTransactionReceipt.get()
  }

  it should "eth_getLogs" taggedAs (PrivNet) in new ScenarioSetup {
    val setupContractResponse =
      service.ethSendTransaction(createContract(firstAccount.address, counterEventContract)).send()
    setupContractResponse.getError shouldEqual null

    val setupContractResponse1 =
      service.ethSendTransaction(createContract(firstAccount.address, emitEventContract)).send()
    setupContractResponse1.getError shouldEqual null

    // Mine 2 Contract creation transactions
    val minedBlock = service.blockFlowable(false).blockingFirst()

    // Get receipt for both contract creation transactions
    val receiptResponse = service.ethGetTransactionReceipt(setupContractResponse.getTransactionHash).send()
    receiptResponse.getTransactionReceipt.isPresent shouldBe true
    val receipt = receiptResponse.getTransactionReceipt.get()

    val receiptResponse1 = service.ethGetTransactionReceipt(setupContractResponse1.getTransactionHash).send()
    receiptResponse1.getTransactionReceipt.isPresent shouldBe true
    val receipt2 = receiptResponse1.getTransactionReceipt.get()

    val updateValue = 5
    val emitValue = 10
    // Call contracts which emits logs
    val counterResponse = service
      .ethSendTransaction(
        contractCall(
          firstAccount.address,
          receipt.getContractAddress,
          writeContract(updateValue, incrementEventContract)
        )
      )
      .send()
    val emiterResponse = service
      .ethSendTransaction(
        contractCall(firstAccount.address, receipt2.getContractAddress, writeContract(emitValue, emitEvent))
      )
      .send()

    val minedBlock1 = service.blockFlowable(false).blockingFirst()

    // All filter which maps to test cases
    // Used simple log filter that includes all txs, expected all txs logs to be shown
    val filter =
      new EthFilter(getBlockParam(minedBlock1.getBlock.getNumber), latestBlock, List(firstAccount.address).asJava)

    // Used log filter with higher fromBlock than toBlock, expected no change to be included
    val badFilter =
      new EthFilter(
        getBlockParam(minedBlock1.getBlock.getNumber.add(1)),
        getBlockParam(minedBlock1.getBlock.getNumber.subtract(1)),
        List(firstAccount.address).asJava
      )

    // Used log filter that filters by event, expected only events selected to be shown
    val particularEventTopic =
      new EthFilter(getBlockParam(minedBlock1.getBlock.getNumber), latestBlock, List(firstAccount.address).asJava)
    particularEventTopic.addSingleTopic(counterContractEventHash)

    // Used log filter with topic length larger than logs, expected no change to be included
    val toLongTopic =
      new EthFilter(getBlockParam(minedBlock1.getBlock.getNumber), latestBlock, List(firstAccount.address).asJava)
    toLongTopic
      .addSingleTopic(counterContractEventHash)
      .addSingleTopic(createTopic(firstAccount.address.drop(2)))
      .addSingleTopic(createTopic(updateValue.toString))
      .addSingleTopic(createTopic(updateValue.toString))

    // Used log filter incluiding various contracts, expected changes from all contracts
    val variousContracts =
      new EthFilter(
        getBlockParam(minedBlock1.getBlock.getNumber),
        latestBlock,
        List(receipt.getContractAddress, receipt2.getContractAddress).asJava
      )

    // Used log filter including only a null topic, expected changes to include all events
    val nullTopic =
      new EthFilter(
        getBlockParam(minedBlock1.getBlock.getNumber),
        latestBlock,
        List(receipt.getContractAddress, receipt2.getContractAddress).asJava
      )
    nullTopic.addNullTopic()

    val response = service.ethGetLogs(filter).send()
    val logs = getLogs(response)
    logs.size shouldEqual 2

    val badFilterResponse = service.ethGetLogs(badFilter).send()
    val badFilterLogs = getLogs(badFilterResponse)
    badFilterLogs.size shouldEqual 0

    val particularEventResponse = service.ethGetLogs(particularEventTopic).send()
    val particularEventLogs = getLogs(particularEventResponse)
    particularEventLogs.size shouldEqual 1

    val toLongTopicResponse = service.ethGetLogs(toLongTopic).send()
    val toLongTopicLogs = getLogs(toLongTopicResponse)
    toLongTopicLogs.size shouldEqual 0

    val variousContractsResponse = service.ethGetLogs(variousContracts).send()
    val variousContractsLogs = getLogs(variousContractsResponse)
    variousContractsLogs.size shouldEqual 2

    val nullTopicResponse = service.ethGetLogs(nullTopic).send()
    val nullTopicLogs = getLogs(nullTopicResponse)
    nullTopicLogs.size shouldEqual 2
  }

  it should "net_peerCount on privnet" taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.netPeerCount().send()
    response.getQuantity.asBigInt shouldEqual 0
  }

  (it should "eth_syncing on privnet").taggedAs(PrivNet, PrivNetNoMining) in new ScenarioSetup {
    val response = service.ethSyncing().send()
    response.isSyncing shouldEqual false
  }

  // test case really similar to eth_Logs but now filters are installed via id
  it should "eth_newFilter and eth_getFilterLogs" taggedAs (PrivNet) in new ScenarioSetup {
    val blockFilter = service.ethNewBlockFilter().send()
    val blockFilterId = blockFilter.getFilterId

    val pendingTransactionFilter = service.ethNewPendingTransactionFilter().send()
    val pendingTransactionFilterId = pendingTransactionFilter.getFilterId

    val setupContractResponse =
      service.ethSendTransaction(createContract(firstAccount.address, counterEventContract)).send()
    setupContractResponse.getError shouldEqual null

    val setupContractResponse1 =
      service.ethSendTransaction(createContract(firstAccount.address, emitEventContract)).send()
    setupContractResponse1.getError shouldEqual null

    // Mine 2 Contract creation transactions
    val unused = service.blockFlowable(false).take(2).blockingLast()

    // Get receipt for both contract creation transactions
    val receiptResponse = service.ethGetTransactionReceipt(setupContractResponse.getTransactionHash).send()
    receiptResponse.getTransactionReceipt.isPresent shouldBe true
    val receipt = receiptResponse.getTransactionReceipt.get()

    val receiptResponse1 = service.ethGetTransactionReceipt(setupContractResponse1.getTransactionHash).send()
    receiptResponse1.getTransactionReceipt.isPresent shouldBe true
    val receipt2 = receiptResponse1.getTransactionReceipt.get()

    val updateValue = 5
    val emitValue = 10
    // Call contracts which emits logs
    val counterResponse = service
      .ethSendTransaction(
        contractCall(
          firstAccount.address,
          receipt.getContractAddress,
          writeContract(updateValue, incrementEventContract)
        )
      )
      .send()
    val emiterResponse = service
      .ethSendTransaction(
        contractCall(firstAccount.address, receipt2.getContractAddress, writeContract(emitValue, emitEvent))
      )
      .send()

    val pendingTransactionLogs = service.ethGetFilterLogs(pendingTransactionFilterId).send()
    pendingTransactionLogs.getLogs.asScala.size shouldEqual 2

    val minedBlock1 = service.blockFlowable(false).blockingFirst()

    // All filter which maps to test cases
    // Used simple log filter that includes all txs, expected all txs logs to be shown
    val filter =
      new EthFilter(getBlockParam(minedBlock1.getBlock.getNumber), latestBlock, List(firstAccount.address).asJava)

    // Used log filter with higher fromBlock than toBlock, expected no change to be included
    val badFilter =
      new EthFilter(
        getBlockParam(minedBlock1.getBlock.getNumber.add(1)),
        getBlockParam(minedBlock1.getBlock.getNumber.subtract(1)),
        List(firstAccount.address).asJava
      )

    // Used log filter that filters by event, expected only events selected to be shown
    val particularEventTopic =
      new EthFilter(getBlockParam(minedBlock1.getBlock.getNumber), latestBlock, List(firstAccount.address).asJava)
    particularEventTopic.addSingleTopic(counterContractEventHash)

    // Used log filter with topic length larger than logs, expected no change to be included
    val toLongTopic =
      new EthFilter(getBlockParam(minedBlock1.getBlock.getNumber), latestBlock, List(firstAccount.address).asJava)
    toLongTopic
      .addSingleTopic(counterContractEventHash)
      .addSingleTopic(createTopic(firstAccount.address.drop(2)))
      .addSingleTopic(createTopic(updateValue.toString))
      .addSingleTopic(createTopic(updateValue.toString))

    // Used log filter incluiding various contracts, expected changes from all contracts
    val variousContracts =
      new EthFilter(
        getBlockParam(minedBlock1.getBlock.getNumber),
        latestBlock,
        List(receipt.getContractAddress, receipt2.getContractAddress).asJava
      )

    // Used log filter including only a null topic, expected changes to include all events
    val nullTopic =
      new EthFilter(
        getBlockParam(minedBlock1.getBlock.getNumber),
        latestBlock,
        List(receipt.getContractAddress, receipt2.getContractAddress).asJava
      )
    nullTopic.addNullTopic()

    // Install All filters
    val installFilterId = service.ethNewFilter(filter).send().getFilterId
    val badFilterId = service.ethNewFilter(badFilter).send().getFilterId
    val particularEventTopicId = service.ethNewFilter(particularEventTopic).send().getFilterId
    val toLongTopicId = service.ethNewFilter(toLongTopic).send().getFilterId
    val variousContractsId = service.ethNewFilter(variousContracts).send().getFilterId
    val nullTopicId = service.ethNewFilter(nullTopic).send().getFilterId

    // BlockFilter shoul be empty
    val blockFilterResponse = service.ethGetFilterLogs(blockFilterId).send()
    blockFilterResponse.getLogs.asScala.size shouldEqual 0

    val response = service.ethGetFilterLogs(installFilterId).send()
    val logs = getLogs(response)
    logs.size shouldEqual 2

    val badFilterResponse = service.ethGetFilterLogs(badFilterId).send()
    val badFilterLogs = getLogs(badFilterResponse)
    badFilterLogs.size shouldEqual 0

    val particularEventResponse = service.ethGetFilterLogs(particularEventTopicId).send()
    val particularEventLogs = getLogs(particularEventResponse)
    particularEventLogs.size shouldEqual 1

    val toLongTopicResponse = service.ethGetFilterLogs(toLongTopicId).send()
    val toLongTopicLogs = getLogs(toLongTopicResponse)
    toLongTopicLogs.size shouldEqual 0

    val variousContractsResponse = service.ethGetFilterLogs(variousContractsId).send()
    val variousContractsLogs = getLogs(variousContractsResponse)
    variousContractsLogs.size shouldEqual 2

    val nullTopicResponse = service.ethGetFilterLogs(nullTopicId).send()
    val nullTopicLogs = getLogs(nullTopicResponse)
    nullTopicLogs.size shouldEqual 2

    val nonExistingFilterChanges = service.ethGetFilterChanges(1).send()
    nonExistingFilterChanges.getLogs.asScala.size shouldEqual 0
  }

  it should "eth_newBlockFilter" taggedAs (PrivNet) in new ScenarioSetup {
    val blockFilter = service.ethNewBlockFilter().send()
    val filterid = blockFilter.getFilterId

    val minedBlock = service.blockFlowable(false).blockingFirst()

    val changes = service.ethGetFilterChanges(filterid).send()
    val logs = changes.getLogs.asScala.toList
    val addedBlocks = logs.map(log => log.asInstanceOf[Hash].get)

    addedBlocks should contain(minedBlock.getBlock.getHash)
  }

  it should "eth_newPendingTransactionFilter" taggedAs (PrivNet) in new ScenarioSetup {
    val transactionFilter = service.ethNewPendingTransactionFilter().send()
    val filterid = transactionFilter.getFilterId

    val setupContractResponse =
      service.ethSendTransaction(createContract(firstAccount.address, counterEventContract)).send()
    setupContractResponse.getError shouldEqual null

    val setupContractResponse1 =
      service.ethSendTransaction(createContract(firstAccount.address, emitEventContract)).send()
    setupContractResponse1.getError shouldEqual null

    val changes = service.ethGetFilterChanges(filterid).send()
    val pendingTransactions = changes.getLogs.asScala.toList.map(log => log.asInstanceOf[Hash].get)

    pendingTransactions should contain theSameElementsAs List(
      setupContractResponse.getTransactionHash,
      setupContractResponse1.getTransactionHash
    )
  }

  it should "eth_uninstallFilter" taggedAs (PrivNet) in new ScenarioSetup {
    val filter = new EthFilter(earliestBlock, latestBlock, List(firstAccount.address).asJava)
    val txFilter = service.ethNewFilter(filter).send()
    val logFilter = txFilter.getFilterId

    val uninstalTxFilterResponse = service.ethUninstallFilter(logFilter).send()
    uninstalTxFilterResponse.isUninstalled shouldEqual true

    //parity version, geth returns true
    val uninstalTxFilterResponse1 = service.ethUninstallFilter(logFilter).send()
    uninstalTxFilterResponse1.isUninstalled shouldEqual true

    // Uninstall block filter
    val blockFilter = service.ethNewBlockFilter().send()
    val blockFilterid = blockFilter.getFilterId
    val minedBlock = service.blockFlowable(false).take(2).blockingLast()
    val blockchanges = service.ethGetFilterChanges(blockFilterid).send()
    val addedBlocks = blockchanges.getLogs.asScala.toList.map(log => log.asInstanceOf[Hash].get)
    addedBlocks should contain(minedBlock.getBlock.getHash)
    val uninstalTxFilterResponse3 = service.ethUninstallFilter(blockFilterid).send()
    uninstalTxFilterResponse.isUninstalled shouldEqual true
    val minedBlock1 = service.blockFlowable(false).take(2).blockingLast()
    val blockchanges1 = service.ethGetFilterChanges(blockFilterid).send()
    blockchanges1.getLogs.asScala.toList.size shouldEqual 0

    // Uninstall pending filter
    val transactionFilter = service.ethNewPendingTransactionFilter().send()
    val transactionFilterId = transactionFilter.getFilterId
    val setupContractResponse =
      service.ethSendTransaction(createContract(firstAccount.address, counterEventContract)).send()
    setupContractResponse.getError shouldEqual null
    val changes = service.ethGetFilterChanges(transactionFilterId).send()
    val pendingTransactions = changes.getLogs.asScala.toList.map(log => log.asInstanceOf[Hash].get)
    pendingTransactions.size shouldEqual 1
    val uninstalPendingFilter = service.ethUninstallFilter(transactionFilterId).send()
    uninstalTxFilterResponse1.isUninstalled shouldEqual true
    val changes1 = service.ethGetFilterChanges(transactionFilterId).send()
    val pendingTransactions1 = changes1.getLogs.asScala.toList.map(log => log.asInstanceOf[Hash].get)
    pendingTransactions1.size shouldEqual 0

    val minedBlock2 = service.blockFlowable(false).blockingFirst()

    val receiptResponse = service.ethGetTransactionReceipt(setupContractResponse.getTransactionHash).send()
    receiptResponse.getTransactionReceipt.isPresent shouldBe true
    val receipt = receiptResponse.getTransactionReceipt.get()

    val updateValue = 5
    // Call contracts which emits logs
    val counterResponse = service
      .ethSendTransaction(
        contractCall(
          firstAccount.address,
          receipt.getContractAddress,
          writeContract(updateValue, incrementEventContract)
        )
      )
      .send()
    val minedBlock3 = service.blockFlowable(false).blockingFirst()

    // log filter was uninstalled
    val logsResponse = service.ethGetFilterChanges(logFilter).send()
    val logs = getLogs(logsResponse)
    logs.size shouldEqual 0
  }

  it should "eth_getWork and eth_submitWork" taggedAs (PrivNetNoMining) in new ScenarioSetup {
    val currentBlock = service.ethGetBlockByNumber(latestBlock, false).send().getBlock
    val workResponse = service.ethGetWork().send()
    val pendingBlockR = service.ethGetBlockByNumber(pendingBlock, false).send().getBlock

    pendingBlockR.getNumber.asBigInt shouldEqual currentBlock.getNumber.asBigInt + 1

    Thread.sleep(2000)

    val workResponse1 = service.ethGetWork().send()
    val pendingBlockR1 = service.ethGetBlockByNumber(pendingBlock, false).send().getBlock
    workResponse.getCurrentBlockHeaderPowHash should not equal workResponse1.getCurrentBlockHeaderPowHash
    pendingBlockR.getTimestamp.asBigInt should not equal pendingBlockR1.getTimestamp.asBigInt

    val submitResponse =
      service.ethSubmitWork(currentBlock.getNonceRaw, currentBlock.getHash, currentBlock.getHash).send()
    submitResponse.solutionValid() shouldEqual false

    val fakeNonce = "0x1233452345"
    val fakeHash = "0xa23"
    val fakeMix = "0xa11"

    val submitResponse1 = service.ethSubmitWork(fakeNonce, fakeHash, fakeMix).send()
    submitResponse1.solutionValid() shouldEqual false
  }

  it should "eth_submitHashRate" taggedAs (PrivNetNoMining) in new ScenarioSetup {
    val hashRate1 = "0x0000000000000000000000000000000000000000000000000000000000500000"
    val hashRate2 = "0x0000000000000000000000000000000000000000000000000000000000600000"
    val id = "0x59daa26581d0acd1fce254fb7e85952f4c09d0915afd33d3886cd914bc7d283c"
    val id2 = "0x59daa26581d0acd1fce254fb7e85952f4c09d0915afd33d3886cd914bc7d283d"

    val hashRateResponse = service.ethSubmitHashrate(hashRate1, id).send()
    hashRateResponse.submissionSuccessful() shouldEqual true

    val getHashRateResponse = service.ethHashrate().send()
    getHashRateResponse.getHashrate.asBigInt shouldEqual hexToBigInt(hashRate1)

    val hashRateResponse1 = service.ethSubmitHashrate(hashRate1, id2).send()
    hashRateResponse1.submissionSuccessful() shouldEqual true

    val getHashRateResponse1 = service.ethHashrate().send()
    getHashRateResponse1.getHashrate.asBigInt shouldEqual hexToBigInt(hashRate1) * 2

    val hashRateResponse2 = service.ethSubmitHashrate(hashRate2, id2).send()
    hashRateResponse2.submissionSuccessful() shouldEqual true

    val getHashRateResponse2 = service.ethHashrate().send()
    getHashRateResponse2.getHashrate.asBigInt shouldEqual hexToBigInt(hashRate1) + hexToBigInt(hashRate2)

    //TimeOut of hashrate
    Thread.sleep(5500)
    val getHashRateResponse3 = service.ethHashrate().send()
    getHashRateResponse3.getHashrate.asBigInt shouldEqual 0
  }

  it should "eth_getTransactionCount (without pending block) without mining" taggedAs (PrivNetNoMining) in new ScenarioSetup {
    val unlockAccountResponse = service.personalUnlockAccount(firstAccount.address, firstAccount.password, 0).send()
    unlockAccountResponse.accountUnlocked() shouldEqual true

    val currentCountResponse = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val currentCount = currentCountResponse.getTransactionCount

    val transferAmount = 100
    val response1 =
      service.ethSendTransaction(valueTransfer(firstAccount.address, secondAccount.address, transferAmount)).send()
    response1.getError shouldEqual null

    val txCountResponse = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val txCount = txCountResponse.getTransactionCount

    txCount.asBigInt shouldEqual currentCount.asBigInt
  }

  it should "eth_getTransactionCount (with pending block) without mining" taggedAs (PrivNetNoMining) in new ScenarioSetup {
    val unlockAccountResponse = service.personalUnlockAccount(firstAccount.address, firstAccount.password, 0).send()
    unlockAccountResponse.accountUnlocked() shouldEqual true

    val currentCountResponse = service.ethGetTransactionCount(firstAccount.address, latestBlock).send()
    val currentCount = currentCountResponse.getTransactionCount

    val transferAmount = 100
    val response1 =
      service.ethSendTransaction(valueTransfer(firstAccount.address, secondAccount.address, transferAmount)).send()
    response1.getError shouldEqual null

    val txCountResponseWithPendingBlock = service.ethGetTransactionCount(firstAccount.address, pendingBlock).send()
    val txCount = txCountResponseWithPendingBlock.getTransactionCount

    txCount.asBigInt shouldEqual currentCount.asBigInt
  }

  it should "personal_ListAccounts and eth_ListAccounts " taggedAs (PrivNet) in new ScenarioSetup {
    val response = service.personalListAccounts().send()
    val personalAccounts = response.getAccountIds.asScala

    personalAccounts should contain.allOf(firstAccount.address, secondAccount.address, thirdAccount.address, gethAccount.address)

    val response1 = service.ethAccounts().send()
    val ethAccounts = response1.getAccounts.asScala

    ethAccounts should contain theSameElementsAs personalAccounts
  }

  // There is no api to delete account, so to avoid creating a lot of unnecessery key stores several methods in one test
  it should "personal_newAccount and personal_unlockAccount and personal_sendTransaction" taggedAs (PrivNet) in new ScenarioSetup {
    val acc1Pass = "1234567"
    val response = service.personalNewAccount(acc1Pass).send()
    val newAccId = response.getAccountId

    val response1 = service.personalNewAccount("asdjhf lkljds").send()
    val newAccId1 = response1.getAccountId

    val response2 = service.personalListAccounts().send()
    val accounts = response2.getAccountIds.asScala
    accounts should contain.allOf(newAccId, newAccId1)

    val transfer1 = service.ethSendTransaction(valueTransfer(firstAccount.address, newAccId, 1000)).send()

    val mined = service.transactionFlowable().blockingFirst()

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

abstract class ScenarioSetup {
  val testConfig = RpcTestConfig("test.conf")

  // Some data from mantis config (this data is not exposed to built version so it is safe to load it here
  val config = ConfigFactory.load("application.conf").getConfig("mantis")
  val clientVersion: String = io.iohk.ethereum.utils.Config.clientVersion
  val protocolVersion = config.getConfig("network").getInt("protocol-version")
  //

  val service = Admin.build(new HttpService(testConfig.mantisUrl))
  val unexisitingBlockHash = "0xaaaaaaaaaaa959b3db6469104c59b803162cf37a23293e8df306e559218f5c6f"
  val badHash = "0xm"
  val emptyResponse = "0x"
  val generalErrorCode = -32602

  val futureBlock = DefaultBlockParameter.valueOf(BigInt(50000000000000L).bigInteger)
  val latestBlock = DefaultBlockParameter.valueOf("latest")
  val pendingBlock = DefaultBlockParameter.valueOf("pending")
  val earliestBlock = DefaultBlockParameter.valueOf("earliest")

  def getBlockParam(number: BigInt): DefaultBlockParameter = {
    DefaultBlockParameter.valueOf(number)
  }

  def getLogs(logResponse: EthLog): List[LogObject] = {
    logResponse.getLogs.asScala.toList.map(log => log.asInstanceOf[LogObject])
  }

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

  // Helpers to provide some meaningful naming in tests
  def createContract(address: String, code: String, gasLimit: Option[BigInteger] = None): Transaction = {
    new Transaction(
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
    new Transaction(
      address,
      null,
      null,
      gasLimit.orNull,
      to,
      null,
      data
    )
  }

  def valueTransfer(
      from: String,
      to: String,
      amount: BigInt,
      nonce: Option[BigInteger] = None,
      gasLimit: Option[BigInteger] = None
  ): Transaction = {
    new Transaction(
      from,
      nonce.orNull,
      null,
      gasLimit.orNull,
      to,
      amount,
      null
    )
  }

  val sampleTransaction = createContract(firstAccount.address, testContract)

  // helper to setup two accounts with same nonce and some initial funding
  def setupTwoNewAccounts(fundsProvider: String, amount: BigInt): (TestAccount, TestAccount) = {
    val first = service.personalNewAccount("").send().getAccountId
    val second = service.personalNewAccount("").send().getAccountId

    val firstUnlock = service.personalUnlockAccount(first, "", 0).send()
    val secondUnlock = service.personalUnlockAccount(second, "", 0).send()

    val trans = service.ethSendTransaction(valueTransfer(fundsProvider, first, amount)).send()
    val trans1 = service.ethSendTransaction(valueTransfer(fundsProvider, second, amount)).send()

    // wait for mine
    val block = service.blockFlowable(false).blockingFirst()

    (TestAccount(first, "", amount), TestAccount(second, "", amount))
  }

  // Needed to sign transaction and send raw transactions
  val keyStoreConfig = KeyStoreConfig.customKeyStoreConfig(testConfig.keystoreDir)

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
      nonce: BigInt
  ): String = {
    val fromAddress = Address(fromAccount.address)
    val fromWallet = getAccountWallet(fromAccount.address, fromAccount.password)

    val req = TransactionRequest(
      from = fromAddress,
      to = toAccount.map(acc => Address(acc.address)),
      value = value,
      data = data,
      nonce = Some(nonce)
    )

    val transaction = req.toTransaction(0)

    val stx = fromWallet.signTx(transaction, None)
    Hex.toHexString(rlp.encode(stx.tx.toRLPEncodable))
  }
}
