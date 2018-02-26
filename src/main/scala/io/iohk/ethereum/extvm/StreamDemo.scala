package io.iohk.ethereum.extvm

import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }
import akka.actor.ActorSystem
import akka.util.ByteString
import scala.concurrent._
import scala.concurrent.duration._
import java.nio.file.Paths

object StreamDemo extends App {
  implicit val system = ActorSystem("demo")
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val source: Source[Int, NotUsed] = Source(1 to 100)
  val done = source.runForeach(i => println(i))
  println("source completed")
  done.onComplete(_ => system.terminate())
}
