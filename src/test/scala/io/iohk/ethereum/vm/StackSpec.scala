package io.iohk.ethereum.vm

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.domain.UInt256

class StackSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  val maxStackSize = 32
  val stackGen: Gen[Stack] = Generators.getStackGen(maxSize = maxStackSize)
  val intGen: Gen[Int] = Gen.choose(0, maxStackSize).filter(_ >= 0)
  val uint256Gen: Gen[UInt256] = Generators.getUInt256Gen()
  val uint256ListGen: Gen[List[UInt256]] = Generators.getListGen(0, 16, uint256Gen)
  val fullStackGen: Gen[Stack] = intGen.flatMap(n => Generators.getStackGen(n, n, uint256Gen, n))
  val nonFullStackGen: Gen[Stack] =
    Generators.getStackGen(maxElems = maxStackSize - 1, maxSize = maxStackSize, valueGen = uint256Gen)

  test("pop single element") {
    forAll(stackGen) { stack =>
      val (v, stack1) = stack.pop
      if (stack.size > 0) {
        v shouldEqual stack.toSeq.head
        stack1.toSeq shouldEqual stack.toSeq.tail
      } else {
        v shouldEqual 0
        stack1 shouldEqual stack
      }
    }
  }

  test("pop single element from an empty stack") {
    forAll(intGen.map(Stack.empty)) { emptyStack =>
      val (value, newStack) = emptyStack.pop
      value shouldEqual UInt256.Zero
      newStack should be(emptyStack)
    }
  }

  test("pop multiple elements") {
    forAll(stackGen, intGen) { (stack, i) =>
      val (vs, stack1) = stack.pop(i)
      if (stack.size >= i) {
        vs shouldEqual stack.toSeq.take(i)
        stack1.toSeq shouldEqual stack.toSeq.drop(i)
      } else {
        vs shouldEqual Seq.fill(i)(UInt256.Zero)
        stack1 shouldEqual stack
      }
    }
  }

  test("push single element") {
    forAll(nonFullStackGen, uint256Gen) { (stack, v) =>
      val stack1 = stack.push(v)

      stack1.toSeq shouldEqual (v +: stack.toSeq)
    }
  }

  test("push single element to full stack") {
    forAll(fullStackGen, uint256Gen) { (stack, v) =>
      val newStack = stack.push(v)

      newStack shouldBe stack
    }
  }

  test("push multiple elements") {
    forAll(stackGen, uint256ListGen) { (stack, vs) =>
      val stack1 = stack.push(vs)

      if (stack.size + vs.size <= stack.maxSize) {
        stack1.toSeq shouldEqual (vs.reverse ++ stack.toSeq)
      } else {
        stack1 shouldEqual stack
      }
    }
  }

  test("duplicate element") {
    forAll(stackGen, intGen) { (stack, i) =>
      val stack1 = stack.dup(i)

      if (i < stack.size && stack.size < stack.maxSize) {
        val x = stack.toSeq(i)
        stack1.toSeq shouldEqual (x +: stack.toSeq)
      } else {
        stack1 shouldEqual stack
      }
    }
  }

  test("swap elements") {
    forAll(stackGen, intGen) { (stack, i) =>
      val stack1 = stack.swap(i)

      if (i < stack.size) {
        val x = stack.toSeq.head
        val y = stack.toSeq(i)
        stack1.toSeq shouldEqual stack.toSeq.updated(0, y).updated(i, x)
      } else {
        stack1 shouldEqual stack
      }
    }
  }

}
