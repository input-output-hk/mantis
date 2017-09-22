package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.rlp.RLP
import io.iohk.ethereum.utils.Logger
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.spongycastle.util.encoders.Hex

import scala.collection.mutable.ListBuffer

class StackSpecComp extends FunSuite with Matchers with PropertyChecks with Logger {

  val emptyStack = Stack.empty()
  val emptyStackL = StackL.empty()

  val stackSwap = (1 to 1023).toList.map(UInt256(_))

  val fullStack = Stack.empty().push(stackSwap)
  val fullStackL = StackL.empty().push(stackSwap)

  val start = 0
  val end = 16
  val iterations = 100000
  val results = 50000

  test("Stack Swap Comparision Test") {
    import scala.collection.mutable.ListBuffer
    var measures = new ListBuffer[Long]()
    var measuresList = new ListBuffer[Long]()
    (1 to iterations).foreach{n =>
      val r = scala.util.Random
      val r1 = start + r.nextInt(( end - start) + 1)

      val start1: Long = System.nanoTime()
      val newStack = fullStack.swap(r1)
      val end1: Long = System.nanoTime()

      val start2: Long = System.nanoTime()
      val newStackList = fullStackL.swap(r1)
      val end2: Long = System.nanoTime()


      newStack.size shouldEqual 1023
      newStackList.size shouldEqual 1023
      val stackTime = end1 - start1
      val stackLTime = end2 - start2
      measures += stackTime
      measuresList += stackLTime
    }



    val statsStack = getStats(measures)
    val statsStackList = getStats(measuresList)
    log.info(s"Time of $results Swap\t: " + sumOfAllRuns(measures) + "ms, for Vector based Stack" )
    log.info(s"Avarage time of vector based Swap is : ${statsStack._1} ns, with dev ${statsStack._2}")
    log.info(s"Median time of vector based Swap is : ${statsStack._3} ns \n")


    log.info(s"Time of $results Swap\t: " + sumOfAllRuns(measuresList) + "ms, for List  based Stack")
    log.info(s"Avarage time of list based Swap is : ${statsStackList._1} ns, with dev ${statsStackList._2}")
    log.info(s"Median time of list based Swap is : ${statsStackList._3} ns \n")
  }

  test("Stack Dup Test") {
    import scala.collection.mutable.ListBuffer
    var measures = new ListBuffer[Long]()
    var measuresList = new ListBuffer[Long]()
    (1 to iterations).foreach{n =>
      val r = scala.util.Random
      val r1 = start + r.nextInt(( end - start) + 1)

      val start1: Long = System.nanoTime()
      val newStack = fullStack.dup(r1)
      val end1: Long = System.nanoTime()

      val start2: Long = System.nanoTime()
      val newStackList = fullStackL.dup(r1)
      val end2: Long = System.nanoTime()

      newStack.size shouldEqual 1024
      newStackList.size shouldEqual 1024
      val stackTime = end1 - start1
      val stackLTime = end2 - start2
      measures += stackTime
      measuresList += stackLTime
    }

    val statsStack = getStats(measures)
    val statsStackList = getStats(measuresList)
    log.info(s"Time of $results Dup\t: " + sumOfAllRuns(measures) + "ms, for Vector based Stack" )
    log.info(s"Avarage time of vector based Dup is : ${statsStack._1} ns, with dev ${statsStack._2}")
    log.info(s"Median time of vector based Dup is : ${statsStack._3} ns \n")


    log.info(s"Time of $results Dup\t: " + sumOfAllRuns(measuresList) + "ms, for List  based Stack")
    log.info(s"Avarage time of list based Dup is : ${statsStackList._1} ns, with dev ${statsStackList._2}")
    log.info(s"Median time of list based Dup is : ${statsStackList._3} ns \n")
  }

  test("Stack Push Almost Full Stack Test") {
    import scala.collection.mutable.ListBuffer
    var measures = new ListBuffer[Long]()
    var measuresList = new ListBuffer[Long]()
    (1 to iterations).foreach{n =>
      val start1: Long = System.nanoTime()
      val newStack = fullStack.push(n)
      val end1: Long = System.nanoTime()

      val start2: Long = System.nanoTime()
      val newStackList = fullStackL.push(n)
      val end2: Long = System.nanoTime()


      newStack.pop._1 shouldEqual n
      newStackList.pop._1 shouldEqual n
      val stackTime = end1 - start1
      val stackLTime = end2 - start2
      measures += stackTime
      measuresList += stackLTime
    }

    val statsStack = getStats(measures)
    val statsStackList = getStats(measuresList)
    log.info(s"Time of $results Push\t: " + sumOfAllRuns(measures) + "ms, for Vector based Stack" )
    log.info(s"Avarage time of vector based Push is : ${statsStack._1} ns, with dev ${statsStack._2}")
    log.info(s"Median time of vector based Push is : ${statsStack._3} ns \n")


    log.info(s"Time of $results Push\t: " + sumOfAllRuns(measuresList) + "ms, for List  based Stack")
    log.info(s"Avarage time of list based Push is : ${statsStackList._1} ns, with dev ${statsStackList._2}")
    log.info(s"Median time of list based Push is : ${statsStackList._3} ns \n")
  }

  test("Stack Push empty Stack Test") {
    import scala.collection.mutable.ListBuffer
    var measures = new ListBuffer[Long]()
    var measuresList = new ListBuffer[Long]()
    (1 to iterations).foreach{n =>
      val start1: Long = System.nanoTime()
      val newStack = emptyStack.push(n)
      val end1: Long = System.nanoTime()

      val start2: Long = System.nanoTime()
      val newStackList = emptyStackL.push(n)
      val end2: Long = System.nanoTime()


      newStack.pop._1 shouldEqual n
      newStackList.pop._1 shouldEqual n
      val stackTime = end1 - start1
      val stackLTime = end2 - start2
      measures += stackTime
      measuresList += stackLTime
    }

    val statsStack = getStats(measures)
    val statsStackList = getStats(measuresList)
    log.info(s"Time of $results Push\t: " + sumOfAllRuns(measures) + "ms, for Vector based Stack" )
    log.info(s"Avarage time of vector based Push is : ${statsStack._1} ns, with dev ${statsStack._2}")
    log.info(s"Median time of vector based Push is : ${statsStack._3} ns \n")


    log.info(s"Time of $results Push\t: " + sumOfAllRuns(measuresList) + "ms, for List  based Stack")
    log.info(s"Avarage time of list based Push is : ${statsStackList._1} ns, with dev ${statsStackList._2}")
    log.info(s"Median time of list based Push is : ${statsStackList._3} ns \n")

  }

  test("Stack Pop Test") {
    import scala.collection.mutable.ListBuffer
    var measures = new ListBuffer[Long]()
    var measuresList = new ListBuffer[Long]()


    (1 to iterations).foreach{n =>
      val start1: Long = System.nanoTime()
      val newStack = fullStack.pop
      val end1: Long = System.nanoTime()


      val start2: Long = System.nanoTime()
      val newStackList = fullStackL.pop
      val end2: Long = System.nanoTime()


      newStack._1 shouldEqual 1023
      newStackList._1 shouldEqual 1023
      val stackTime = end1 - start1
      val stackLTime = end2 - start2
      measures += stackTime
      measuresList += stackLTime
    }

    val statsStack = getStats(measures)
    val statsStackList = getStats(measuresList)
    log.info(s"Time of $results Pop\t: " + sumOfAllRuns(measures) + "ms, for Vector based Stack" )
    log.info(s"Avarage time of vector based Pop is : ${statsStack._1} ns, with dev ${statsStack._2}")
    log.info(s"Median time of vector based Pop is : ${statsStack._3} ns \n")


    log.info(s"Time of $results Pop\t: " + sumOfAllRuns(measuresList) + "ms, for List  based Stack")
    log.info(s"Avarage time of list based Pop is : ${statsStackList._1} ns, with dev ${statsStackList._2}")
    log.info(s"Median time of list based Pop is : ${statsStackList._3} ns \n")
  }

  test("Stack Pop List Test") {
    import scala.collection.mutable.ListBuffer
    var measures = new ListBuffer[Long]()
    var measuresList = new ListBuffer[Long]()

    val minPopValue = 1
    val maxPopValue = 4

    (1 to iterations).foreach{n =>
      val r = scala.util.Random
      val r1 = minPopValue + r.nextInt(( maxPopValue - minPopValue) + 1)


      val start1: Long = System.nanoTime()
      val newStack = fullStack.pop(r1)
      val end1: Long = System.nanoTime()


      val start2: Long = System.nanoTime()
      val newStackList = fullStackL.pop(r1)
      val end2: Long = System.nanoTime()


      newStack._1.length shouldEqual r1
      newStackList._1.length shouldEqual r1
      val stackTime = end1 - start1
      val stackLTime = end2 - start2
      measures += stackTime
      measuresList += stackLTime
    }

    val statsStack = getStats(measures)
    val statsStackList = getStats(measuresList)
    log.info(s"Time of $results Pop\t: " + sumOfAllRuns(measures) + "ms, for Vector based Stack" )
    log.info(s"Avarage time of vector based Pop is : ${statsStack._1} ns, with dev ${statsStack._2}")
    log.info(s"Median time of vector based Pop is : ${statsStack._3} ns \n")


    log.info(s"Time of $results Pop\t: " + sumOfAllRuns(measuresList) + "ms, for List  based Stack")
    log.info(s"Avarage time of list based Pop is : ${statsStackList._1} ns, with dev ${statsStackList._2}")
    log.info(s"Median time of list based Pop is : ${statsStackList._3} ns \n")
  }


  private def getStats(list: ListBuffer[Long]) = {
    val interval = 500
    val mid = results / 2

    val impres = list.takeRight(results).sorted.slice(mid - interval, mid + interval)
    val mean = impres.sum.toDouble / impres.length
    val median = impres(impres.length / 2)

    val variance = impres.map(_.toDouble).map(num => math.pow(num - mean, 2)).sum / impres.length.toDouble
    val dev = math.sqrt(variance)
    (mean, dev, median)
  }

  private def sumOfAllRuns(list: ListBuffer[Long]): Double = {
    list.takeRight(results).sum.toDouble / 1000000.0
  }

}
