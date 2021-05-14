package io.iohk.ethereum

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@State(Scope.Benchmark)
class StringConcatBench {

  val rnd: Random.type = Random
  val size: Int = 1000

  @Benchmark
  def simpleConcat(bh: Blackhole): Unit = {
    val a = rnd.nextString(size)
    val b = rnd.nextString(size)
    val c = a + b
    bh.consume(c)
  }

  @Benchmark
  def richStringConcat(bh: Blackhole): Unit = {
    val a = rnd.nextString(size)
    val b = rnd.nextString(size)
    val c = s"$a$b"
    bh.consume(c)
  }

  @Benchmark
  def stringBuilderConcat(bh: Blackhole): Unit = {
    val a = new StringBuilder(rnd.nextString(size))
    val b = a.append(rnd.nextString(size))
    val c = b.mkString
    bh.consume(c)
  }
}