package io.iohk.ethereum.rpcTest.utils

import cats.effect.Resource
import monix.eval.Task

object ResourceOps {
  def tupleResources[A, B](resourceA: Resource[Task, A], resourceB: Resource[Task, B]): Resource[Task, (A, B)] =
    resourceA.flatMap(a => resourceB.flatMap(b => Resource.pure((a, b))))

  implicit class ResourceOps[A, B](val resource: Resource[Task, (A, B)]) extends AnyVal {
    def use2[T](cb: (A, B) => Task[T]): Task[T] = {
      resource.use {
        case (a, b) => cb(a, b)
      }
    }
  }

  implicit class TupleWithResourcesOps[A, B](val resources: (Resource[Task, A], Resource[Task, B])) extends AnyVal {
    def use2[T](cb: (A, B) => Task[T]): Task[T] = tupleResources(resources._1, resources._2).use2(cb)
  }
}
