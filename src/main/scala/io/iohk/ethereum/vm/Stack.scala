package io.iohk.ethereum.vm

case class Stack(underlying: Vector[DataWord] = Vector()) {

  def pop: (DataWord, Stack) = {
    //TODO: empty stack handling
    val word = underlying.last
    val updated = underlying.dropRight(1)
    (word, Stack(updated))
  }

  def pop(n: Int): (Seq[DataWord], Stack) = {
    //TODO: empty stack handling
    val (updated, popped) = underlying.splitAt(underlying.length - n)
    (popped.reverse, Stack(updated))
  }

  def push(word: DataWord): Stack =
    Stack(underlying :+ word)

  def push(words: Seq[DataWord]): Stack =
    Stack(underlying ++ words)
}
