package io.iohk.ethereum.domain

object ChainWeight {
  //FIXME: a shorter name?
  def totalDifficultyOnly(td: BigInt): ChainWeight =
    ChainWeight(0, td)

  val zero: ChainWeight =
    ChainWeight(0, 0)
}

case class ChainWeight(
    lastCheckpointNumber: BigInt,
    totalDifficulty: BigInt
) extends Ordered[ChainWeight] {

  override def compare(that: ChainWeight): Int =
    this.asTuple.compare(that.asTuple)

  def increase(header: BlockHeader): ChainWeight = {
    val isNewerCheckpoint = header.hasCheckpoint && header.number > lastCheckpointNumber
    val checkpointNum = if (isNewerCheckpoint) header.number else lastCheckpointNumber
    ChainWeight(checkpointNum, totalDifficulty + header.difficulty)
  }

  def asTuple: (BigInt, BigInt) =
    ChainWeight.unapply(this).get

  //Test API

  def increaseTotalDifficulty(td: BigInt): ChainWeight =
    copy(totalDifficulty = totalDifficulty + td)
}
