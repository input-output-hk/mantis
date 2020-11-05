package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.domain.{Address, Block, BlockBody, SignedTransaction, Transaction}
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import mouse.all._

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
    (1 to amount).toList.foldLeft[List[Block]](Nil){ (generated, _) =>
      val parent = generated.lastOption.getOrElse(branchParent)
      generated :+ (parent |> generateBlock |> adjustBlock)
    }

  def generateBlock(parent: Block): Block = {
    val header = parent.header.copy(
      extraData = randomHash(),
      number = parent.number + 1,
      parentHash = parent.hash,
      nonce = ByteString(Random.nextLong())
    )
    val ommer = defaultHeader.copy(extraData = randomHash())
    val tx = defaultTx.copy(payload = randomHash())
    val stx = SignedTransaction.sign(tx, keyPair, None)

    Block(header, BlockBody(List(stx.tx), List(ommer)))
  }

}
