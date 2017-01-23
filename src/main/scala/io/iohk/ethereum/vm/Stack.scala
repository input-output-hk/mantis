package io.iohk.ethereum.vm

object Stack {
  val MaxSize = 1024

  val Empty: Stack = new Stack(Vector())

  def populated(elems: DataWord*): Stack =
    new Stack(elems.toVector, overflown = elems.size > MaxSize)
}

class Stack private(underlying: Vector[DataWord], val underflown: Boolean = false, val overflown: Boolean = false) {

  def pop: (DataWord, Stack) = underlying.lastOption match {
    case Some(word) =>
      val updated = underlying.dropRight(1)
      (word, new Stack(updated))

    case None =>
      (DataWord(0), new Stack(underlying, underflown = true))
  }

  def pop(n: Int): (Seq[DataWord], Stack) = {
    val (updated, popped) = underlying.splitAt(underlying.length - n)
    if (popped.length >= n)
      (popped.reverse, new Stack(updated))
    else
      (popped.reverse.padTo(n, DataWord(0)), new Stack(updated, underflown = true))
  }

  def push(word: DataWord): Stack = {
    val updated = underlying :+ word
    new Stack(updated, overflown = updated.length > Stack.MaxSize)
  }

  def push(words: Seq[DataWord]): Stack = {
    val updated = underlying ++ words
    new Stack(updated, overflown = updated.length > Stack.MaxSize)
  }
}
