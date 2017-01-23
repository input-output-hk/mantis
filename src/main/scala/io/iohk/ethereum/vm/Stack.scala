package io.iohk.ethereum.vm

object Stack {
  val MaxSize = 1024
}

case class Stack(underlying: Vector[DataWord] = Vector()) {

  def pop: Either[ProgramError, (DataWord, Stack)] = underlying.lastOption match {
    case Some(word) =>
      val updated = underlying.dropRight(1)
      Right(word, new Stack(updated))

    case None =>
      Left(StackUnderflow)
  }

  def pop(n: Int): Either[ProgramError, (Seq[DataWord], Stack)] = {
    val (updated, popped) = underlying.splitAt(underlying.length - n)
    if (popped.length >= n)
      Right(popped.reverse, new Stack(updated))
    else
      Left(StackUnderflow)
  }

  def push(word: DataWord): Either[ProgramError, Stack] = {
    val updated = underlying :+ word
    if (updated.length > Stack.MaxSize)
      Right(Stack(updated))
    else
      Left(StackOverflow)
  }

  def push(words: Seq[DataWord]): Either[ProgramError, Stack]  = {
    val updated = underlying ++ words
    if (updated.length > Stack.MaxSize)
      Right(Stack(updated))
    else
      Left(StackOverflow)
  }
}
