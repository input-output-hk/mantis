package io.iohk.ethereum.jsonrpc

import java.time.{Clock, Duration, Instant}

import io.iohk.ethereum.jsonrpc.ExpiringMap.ValueWithDuration

import scala.collection.mutable

object ExpiringMap {

  case class ValueWithDuration[V](value: V, expiration: Instant)

  def empty[K, V](defaultElementRetentionTime: Duration,
                  clock: Clock = Clock.systemUTC()): ExpiringMap[K, V] =
    new ExpiringMap(mutable.Map.empty, clock, defaultElementRetentionTime)
}

class ExpiringMap[K, V](private val underLaying: mutable.Map[K, ValueWithDuration[V]],
                        private val clock: Clock,
                        private val defaultRetentionTime: Duration) {

  def add(k: K, v: V, duration: Duration): ExpiringMap[K, V] = {
    underLaying += k -> ValueWithDuration(v, Instant.now(clock).plus(duration))
    this
  }

  def add(k: K, v: V): ExpiringMap[K, V] = {
    add(k, v, defaultRetentionTime)
  }

  def add(k: K, v: V, duration: Option[Duration]): ExpiringMap[K, V] = {
    duration.fold(add(k, v))(add(k, v, _))
  }

  def remove(k: K): ExpiringMap[K, V] = { underLaying -= k ;  this }

  def get(k: K): Option[V] = {
    underLaying.get(k).flatMap(value =>
      if (Duration.between(value.expiration, Instant.now(clock)).isNegative)
        Some(value.value)
      else {
        underLaying -= k
        None
      }
    )
  }
}

