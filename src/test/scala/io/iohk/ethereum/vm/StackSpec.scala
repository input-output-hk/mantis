package io.iohk.ethereum.vm

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

class StackSpec extends FunSuite with Matchers with PropertyChecks {

  val maxStackSize = 32
  val stackGen = Generators.getStackGen(maxSize = maxStackSize)
  val intGen = Gen.choose(0, Generators.testStackMaxSize)
  val dataWordGen = Generators.getDataWordGen()
  val dataWordListGen = Generators.getListGen(0, 16, dataWordGen)

  test("pop single element") {
    forAll(stackGen) { stack =>
      stack.pop match {
        case Left(StackUnderflow) =>
          stack.size shouldEqual 0

        case Left(err @ StackOverflow) =>
          fail(s"$err should not happen")

        case Right((v, stack1)) =>
          v shouldEqual stack.toSeq.head
          stack1.toSeq shouldEqual stack.toSeq.tail
      }
    }
  }

  test("pop multiple elements") {
    forAll(stackGen, intGen) { (stack, i) =>
      stack.pop(i) match {
        case Left(StackUnderflow) =>
          stack.size should be < i

        case Left(err @ StackOverflow) =>
          fail(s"$err should not happen")

        case Right((seq, stack1)) =>
          seq shouldEqual stack.toSeq.take(i)
          stack1.toSeq shouldEqual stack.toSeq.drop(i)
      }
    }
  }

  test("push single element") {
    forAll(stackGen, dataWordGen) { (stack, v) =>
      stack.push(v) match {
        case Left(StackOverflow) =>
          stack.size shouldEqual stack.maxSize

        case Left(err @ StackUnderflow) =>
          fail(s"$err should not happen")

        case Right(stack1) =>
          stack1.toSeq shouldEqual (v +: stack.toSeq)
      }
    }
  }

  test("push multiple elements") {
    forAll(stackGen, dataWordListGen) { (stack, vs) =>
      stack.push(vs) match {
        case Left(StackOverflow) =>
          stack.size should be > (stack.maxSize - vs.size)

        case Left(err @ StackUnderflow) =>
          fail(s"$err should not happen")

        case Right(stack1) =>
          stack1.toSeq shouldEqual (vs.reverse ++ stack.toSeq)
      }
    }
  }

  test("duplicate element") {
    forAll(stackGen, intGen) { (stack, i) =>
      stack.dup(i) match {
        case Left(StackOverflow) =>
          stack.size shouldEqual stack.maxSize

        case Left(StackUnderflow) =>
          stack.size should be < (i + 1)

        case Right(stack1) =>
          val x = stack.toSeq(i)
          stack1.toSeq shouldEqual (x +: stack.toSeq)
      }
    }
  }

  test("swap elements") {
    forAll(stackGen, intGen) { (stack, i) =>
      stack.swap(i) match {
        case Left(StackUnderflow) =>
          stack.size should be < (i + 1)

        case Left(err @ StackOverflow) =>
          fail(s"$err should not happen")

        case Right(stack1) =>
          val x = stack.toSeq.head
          val y = stack.toSeq(i)
          stack1.toSeq shouldEqual stack.toSeq.updated(0, y).updated(i, x)
      }
    }
  }

}
