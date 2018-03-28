package io.iohk.ethereum.utils

import java.util.concurrent.atomic.AtomicReference

/**
 * An [[java.util.concurrent.atomic.AtomicReference AtomicReference]] that can be set once.
 */
class Ref[T <: AnyRef] {
  private[this] final val ref = new AtomicReference[Option[T]](None)

  // set once (but not necessarily compute once)
  final def setOnce(t: ⇒T): Boolean = ref.get().isEmpty && ref.compareAndSet(None, Some(t))

  final def isDefined: Boolean = ref.get().isDefined
  final def isEmpty: Boolean = ref.get().isEmpty

  final def map[U](f: T ⇒ U): Option[U] = ref.get().map(f)
  final def foreach[U](f: T ⇒ U): Unit = map(f)
}
