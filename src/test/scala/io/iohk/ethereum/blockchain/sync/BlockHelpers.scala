package io.iohk.ethereum.blockchain.sync
import akka.util.ByteString
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.domain._
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.{Fixtures, ObjectGenerators}
import org.bouncycastle.crypto.AsymmetricCipherKeyPair

import scala.math.BigInt
import mouse.all._

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

  def getBlocks(amount: Int, parent: Block, adjustBlock: Block => Block = identity): List[Block] =
    (1 to amount).toList.foldLeft[List[Block]](Nil)((generated, _) => {
      val theParent = generated.lastOption.getOrElse(parent)
      generated :+ (theParent |> getBlock |> adjustBlock)
    })

  def getBlock(nr: BigInt, parent: Block): Block = {
    val header = defaultHeader.copy(extraData = randomHash(), number = nr, parentHash = parent.hash)
    val ommer = defaultHeader.copy(extraData = randomHash())
    val tx = defaultTx.copy(payload = randomHash())
    val stx = SignedTransaction.sign(tx, keyPair, None)

    Block(header, BlockBody(List(stx.tx), List(ommer)))
  }

  def getBlock(parent: Block): Block = getBlock(parent.number + 1, parent)
}
