package io.iohk.ethereum.consensus.atomixraft

import java.util.concurrent.atomic.AtomicReference

class Ref[T <: AnyRef] {
  final val ref = new AtomicReference[Option[T]](None)

  // set once (but not necessarily compute once)
  final def setOnce(t: ⇒T): Boolean = ref.get().isEmpty && ref.compareAndSet(None, Some(t))

  final def isDefined: Boolean = ref.get().isDefined
  final def isEmpty: Boolean = ref.get().isEmpty

  final def run[U](f: T ⇒ U): Option[U] = ref.get().map(f)
}
