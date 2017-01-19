package io.iohk.ethereum.vm

case class Stack(underlying: Vector[DataWord] = Vector()) {

  def pop: (DataWord, Stack) = {
    //TODO: empty stack handling
    val word = underlying.last
    val updated = underlying.dropRight(1)
    (word, Stack(updated))
  }

  def push(word: DataWord): Stack =
    Stack(underlying :+ word)
}
