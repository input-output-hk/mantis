package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.UInt256

object StackL {
  /**
    * Stack max size as defined in the YP (9.1)
    */
  val DefaultMaxSize = 1024

  def empty(maxSize: Int = DefaultMaxSize): StackL =
    new StackL(List(), maxSize)
}

//TODO: consider a List with head being top of the stack (DUP,SWAP go at most the depth of 16) [EC-251]
/**
  * Stack for the EVM. Instruction pop their arguments from it and push their results to it.
  * The Stack doesn't handle overflow and underflow errors. Any operations that trascend given stack bounds will
  * return the stack unchanged. Pop will always return zeroes in such case.
  */
class StackL private(private val underlying: List[UInt256], val maxSize: Int) {

  def pop: (UInt256, StackL) = underlying.headOption match {
    case Some(word) =>
      val updated = underlying.tail
      (word, copy(updated))

    case None =>
      (UInt256.Zero, this)
  }

  /**
    * Pop n elements from the stack. The first element in the resulting sequence will be the top-most element
    * in the current stack
    */
  def pop(n: Int): (Seq[UInt256], StackL) = {
    val (popped, updated) = underlying.splitAt(n)
    if (popped.length == n)
      (popped, copy(updated))
    else
      (Seq.fill(n)(UInt256.Zero), this)
  }

  def push(word: UInt256): StackL = {
    val updated = word :: underlying
    if (updated.length <= maxSize)
      copy(updated)
    else
      this
  }

  /**
    * Push a sequence of elements to the stack. That last element of the sequence will be the top-most element
    * in the resulting stack
    */
  def push(words: Seq[UInt256]): StackL = {
    val updated = underlying.reverse_:::(words.toList)
    if (updated.length > maxSize)
      this
    else
      copy(updated)
  }

  /**
    * Duplicate i-th element of the stack, pushing it to the top. i=0 is the top-most element.
    */
  def dup(i: Int): StackL = {
    if (i < 0 || i >= underlying.length || underlying.length >= maxSize)
      this
    else
      copy(underlying(i) :: underlying)
  }

  /**
    * Swap i-th and the top-most elements of the stack. i=0 is the top-most element (and that would be a no-op)
    */
  def swap(i: Int): StackL = {
    if (i <= 0 || i >= underlying.length)
      this
    else {
      val a = underlying.head
      val b = underlying(i)
      val updated = b :: underlying.updated(i, a).tail
      copy(updated)
    }
  }

  def size: Int = underlying.size

  /**
    * @return the elements of the stack as a sequence, with the top-most element of the stack
    *         as the first element in the sequence
    */
  def toSeq: Seq[UInt256] = underlying

  override def equals(that: Any): Boolean = that match {
    case that: StackL => this.underlying == that.underlying
    case _ => false
  }

  override def hashCode(): Int = underlying.hashCode

  override def toString: String =
    underlying.reverse.mkString("Stack(", ",", ")")

  private def copy(updated: List[UInt256]): StackL =
    new StackL(updated, maxSize)
}
