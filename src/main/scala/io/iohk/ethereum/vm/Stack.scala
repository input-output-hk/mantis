package io.iohk.ethereum.vm

import cats.syntax.either._

object Stack {
  val DefaultMaxSize = 1024

  def empty(maxSize: Int = DefaultMaxSize): Stack =
    new Stack(Vector(), maxSize)
}

//TODO: consider a List with head being top of the stack (DUP,SWAP go at most the depth of 16)
class Stack(underlying: Vector[DataWord], val maxSize: Int) {

  def pop: Either[StackError, (DataWord, Stack)] = underlying.lastOption match {
    case Some(word) =>
      val updated = underlying.dropRight(1)
      (word, copy(updated)).asRight

    case None =>
      StackUnderflow.asLeft
  }

  def pop(n: Int): Either[StackError, (Seq[DataWord], Stack)] = {
    val (updated, popped) = underlying.splitAt(underlying.length - n)
    if (popped.length >= n)
      (popped.reverse, copy(updated)).asRight
    else
      StackUnderflow.asLeft
  }

  def push(word: DataWord): Either[StackError, Stack] = {
    val updated = underlying :+ word
    if (updated.length <= maxSize)
      copy(updated).asRight
    else
      StackOverflow.asLeft
  }

  def push(words: Seq[DataWord]): Either[StackError, Stack] = {
    val updated = underlying ++ words
    if (updated.length > maxSize)
      copy(updated).asRight
    else
      StackOverflow.asLeft
  }

  def dup(i: Int): Either[StackError, Stack] = {
    val j = underlying.length - i - 1

    if (j < 0)
      StackUnderflow.asLeft
    else if (underlying.length >= maxSize)
      StackOverflow.asLeft
    else
      copy(underlying :+ underlying(j)).asRight
  }

  def swap(i: Int): Either[StackError, Stack] = {
    val j = underlying.length - i - 1

    if (j < 0)
      StackUnderflow.asLeft
    else {
      val a = underlying.last
      val b = underlying(j)
      val updated = underlying.updated(j, a).init :+ b
      copy(updated).asRight
    }
  }

  private def copy(updated: Vector[DataWord]): Stack =
    new Stack(updated, maxSize)
}
