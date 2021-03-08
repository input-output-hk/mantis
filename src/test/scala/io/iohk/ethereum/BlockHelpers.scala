package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields
import io.iohk.ethereum.domain._
import io.iohk.ethereum.security.SecureRandomBuilder
import mouse.all._
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

import scala.util.Random

object BlockHelpers extends SecureRandomBuilder {

  // scalastyle:off magic.number
  val defaultHeader = Fixtures.Blocks.ValidBlock.header.copy(
    difficulty = 1000000,
    number = 1,
    gasLimit = 1000000,
    gasUsed = 0,
    unixTimestamp = 0
  )

  val defaultTx = Transaction(
    nonce = 42,
    gasPrice = 1,
    gasLimit = 90000,
    receivingAddress = Address(123),
    value = 0,
    payload = ByteString.empty
  )

  val genesis: Block = Block(defaultHeader.copy(number = 0), BlockBody(Nil, Nil))

  val keyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)

  def randomHash(): ByteString =
    ObjectGenerators.byteStringOfLengthNGen(32).sample.get

  def generateChain(amount: Int, branchParent: Block, adjustBlock: Block => Block = identity): List[Block] =
    (1 to amount).toList.foldLeft[List[Block]](Nil) { (generated, _) =>
      val parent = generated.lastOption.getOrElse(branchParent)
      generated :+ (parent |> generateBlock |> adjustBlock)
    }

  def resetHeaderExtraFields(hef: BlockHeader.HeaderExtraFields): BlockHeader.HeaderExtraFields = hef match {
    case HeaderExtraFields.HefEmpty              => HeaderExtraFields.HefEmpty
    case HeaderExtraFields.HefPostEcip1098(_)    => HeaderExtraFields.HefPostEcip1098(treasuryOptOut = false)
    case HeaderExtraFields.HefPostEcip1097(_, _) => HeaderExtraFields.HefPostEcip1097(treasuryOptOut = false, None)
  }

  def generateBlock(parent: Block): Block = {
    val header = parent.header.copy(
      extraData = randomHash(),
      number = parent.number + 1,
      parentHash = parent.hash,
      nonce = ByteString(Random.nextLong()),
      extraFields = resetHeaderExtraFields(parent.header.extraFields)
    )
    val ommer = defaultHeader.copy(extraData = randomHash())
    val tx = defaultTx.copy(payload = randomHash())
    val stx = SignedTransaction.sign(tx, keyPair, None)

    Block(header, BlockBody(List(stx.tx), List(ommer)))
  }

  def updateHeader(block: Block, updater: BlockHeader => BlockHeader): Block =
    block.copy(header = updater(block.header))

  def withTransactions(block: Block, transactions: List[SignedTransaction]): Block =
    block.copy(body = block.body.copy(transactionList = transactions))

}
