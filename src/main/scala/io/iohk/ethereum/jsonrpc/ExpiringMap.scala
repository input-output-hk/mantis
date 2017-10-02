package io.iohk.ethereum.jsonrpc

import java.time.{Clock, Duration, Instant}

import io.iohk.ethereum.jsonrpc.ExpiringMap.ValueWithDuration

import scala.collection.mutable
import scala.util.Try

object ExpiringMap {

 case class ValueWithDuration[V](value: V, expiration: Instant)

  def empty[K, V](defaultElementRetentionTime: Duration,
                  clock: Clock = Clock.systemUTC()): ExpiringMap[K, V] =
    new ExpiringMap(mutable.Map.empty, clock, defaultElementRetentionTime)
}
/**
  * Simple wrapper around mutable map which enriches each element with expiration time (specified by user or default)
  * Map is passive which means it only check for expiration and remove expired element during get function
  */
class ExpiringMap[K, V] private (val underLaying: mutable.Map[K, ValueWithDuration[V]],
                                 val clock: Clock,
                                 val defaultRetentionTime: Duration) {

  def addUntil(k: K, v: V, until: Instant): ExpiringMap[K, V] = {
    underLaying += k -> ValueWithDuration(v, until)
    this
  }

  def add(k: K, v: V, duration: Duration): ExpiringMap[K, V] = {
    addUntil(k, v, Try(Instant.now(clock).plus(duration)).getOrElse(Instant.MAX))
  }

  def addForever(k: K, v: V): ExpiringMap[K, V] =
    addUntil(k, v, Instant.MAX)

  def add(k: K, v: V): ExpiringMap[K, V] =
    add(k, v, defaultRetentionTime)

  def remove(k: K): ExpiringMap[K, V] = {
    underLaying -= k
    this
  }

  def get(k: K): Option[V] = {
    underLaying.get(k).flatMap(value =>
      if (isNotExpired(value))
        Some(value.value)
      else {
        remove(k)
        None
      }
    )
  }

  private def isNotExpired(value: ValueWithDuration[V]) =
    Duration.between(value.expiration, Instant.now(clock)).isNegative
}

