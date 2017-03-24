package io.iohk.ethereum.vm

object Stack {
  /**
    * Stack max size as defined in the YP (9.1)
    */
  val DefaultMaxSize = 1024

  def empty(maxSize: Int = DefaultMaxSize): Stack =
    new Stack(Vector(), maxSize)
}

//TODO: consider a List with head being top of the stack (DUP,SWAP go at most the depth of 16)
/**
  * Stack for the EVM. Instruction pop their arguments from it and push their results to it.
  * The Stack doesn't handle overflow and underflow errors. Any operations that trascend given stack bounds will
  * return the stack unchanged. Pop will always return zeroes in such case.
  */
class Stack private(private val underlying: Vector[DataWord], val maxSize: Int) {

  def pop: (DataWord, Stack) = underlying.lastOption match {
    case Some(word) =>
      val updated = underlying.dropRight(1)
      (word, copy(updated))

    case None =>
      (DataWord.Zero, this)
  }

  /**
    * Pop n elements from the stack. The first element in the resulting sequence will be the top-most element
    * in the current stack
    */
  def pop(n: Int): (Seq[DataWord], Stack) = {
    val (updated, popped) = underlying.splitAt(underlying.length - n)
    if (popped.length == n)
      (popped.reverse, copy(updated))
    else
      (Seq.fill(n)(DataWord.Zero), this)
  }

  def push(word: DataWord): Stack = {
    val updated = underlying :+ word
    if (updated.length <= maxSize)
      copy(updated)
    else
      this
  }

  /**
    * Push a sequence of elements to the stack. That last element of the sequence will be the top-most element
    * in the resulting stack
    */
  def push(words: Seq[DataWord]): Stack = {
    val updated = underlying ++ words
    if (updated.length > maxSize)
      this
    else
      copy(updated)
  }

  /**
    * Duplicate i-th element of the stack, pushing it to the top. i=0 is the top-most element.
    */
  def dup(i: Int): Stack = {
    val j = underlying.length - i - 1

    if (i < 0 || i >= underlying.length || underlying.length >= maxSize)
      this
    else
      copy(underlying :+ underlying(j))
  }

  /**
    * Swap i-th and the top-most elements of the stack. i=0 is the top-most element (and that would be a no-op)
    */
  def swap(i: Int): Stack = {
    val j = underlying.length - i - 1

    if (i <= 0 || i >= underlying.length)
      this
    else {
      val a = underlying.last
      val b = underlying(j)
      val updated = underlying.updated(j, a).init :+ b
      copy(updated)
    }
  }

  def size: Int = underlying.size

  /**
    * @return the elements of the stack as a sequence, with the top-most element of the stack
    *         as the first element in the sequence
    */
  def toSeq: Seq[DataWord] = underlying.reverse

  override def equals(that: Any): Boolean = that match {
    case that: Stack => this.underlying == that.underlying
    case _ => false
  }

  override def hashCode(): Int = underlying.hashCode

  override def toString: String =
    underlying.reverse.mkString("Stack(", ",", ")")

  private def copy(updated: Vector[DataWord]): Stack =
    new Stack(updated, maxSize)
}
