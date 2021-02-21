package io.iohk.ethereum

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest.{BeforeAndAfterAll, Suite}

trait WithActorSystemShutDown extends BeforeAndAfterAll { this: Suite =>
  implicit val system: ActorSystem

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
  }
}
