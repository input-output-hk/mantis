package io.iohk.ethereum.vm

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

class StackSpec extends FunSuite with Matchers with PropertyChecks {

  val maxStackSize = 32
  val stackGen = Generators.getStackGen(maxSize = maxStackSize)
  val intGen = Gen.choose(0, maxStackSize).filter(_ >= 0)
  val dataWordGen = Generators.getDataWordGen()
  val dataWordListGen = Generators.getListGen(0, 16, dataWordGen)

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

  test("pop multiple elements") {
    forAll(stackGen, intGen) { (stack, i) =>
      val (vs, stack1) = stack.pop(i)
      if (stack.size >= i) {
        vs shouldEqual stack.toSeq.take(i)
        stack1.toSeq shouldEqual stack.toSeq.drop(i)
      } else {
        vs shouldEqual Seq.fill(i)(DataWord.Zero)
        stack1 shouldEqual stack
      }
    }
  }

  test("push single element") {
    forAll(stackGen, dataWordGen) { (stack, v) =>
      val stack1 = stack.push(v)

      if (stack.size < stack.maxSize) {
        stack1.toSeq shouldEqual (v +: stack.toSeq)
      } else {
        stack1 shouldEqual stack
      }
    }
  }

  test("push multiple elements") {
    forAll(stackGen, dataWordListGen) { (stack, vs) =>
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
