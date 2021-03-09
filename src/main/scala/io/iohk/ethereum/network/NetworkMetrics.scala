package io.iohk.ethereum.network

import io.iohk.ethereum.metrics.MetricsContainer
import io.micrometer.core.instrument.Counter

import java.util.concurrent.atomic.AtomicLong

case object NetworkMetrics extends MetricsContainer {

  final private val HandshakedIncomingPeersGauge =
    metrics.registry.gauge("network.peers.incoming.handshaked.gauge", new AtomicLong(0))
  final private val HandshakedOutgoingPeersGauge =
    metrics.registry.gauge("network.peers.outgoing.handshaked.gauge", new AtomicLong(0))

  final val ReceivedMessagesCounter: Counter = metrics.counter("network.messages.received.counter")

  final val SentMessagesCounter: Counter = metrics.counter("network.messages.sent.counter")

  final val DiscoveredPeersSize: AtomicLong = metrics.registry.gauge("network.discovery.foundPeers.gauge", new AtomicLong(0))

  final val BlacklistedPeersSize: AtomicLong = metrics.registry.gauge("network.peers.blacklisted.gauge", new AtomicLong(0))

  final val PendingPeersSize: AtomicLong = metrics.registry.gauge("network.peers.pending.gauge", new AtomicLong(0))

  def registerAddHandshakedPeer(peer: Peer): Unit =
    if (peer.incomingConnection) {
      HandshakedIncomingPeersGauge.incrementAndGet()
    } else {
      HandshakedOutgoingPeersGauge.incrementAndGet()
    }

  def registerRemoveHandshakedPeer(peer: Peer): Unit =
    if (peer.incomingConnection) {
      HandshakedIncomingPeersGauge.decrementAndGet()
    } else {
      HandshakedOutgoingPeersGauge.decrementAndGet()
    }

}
