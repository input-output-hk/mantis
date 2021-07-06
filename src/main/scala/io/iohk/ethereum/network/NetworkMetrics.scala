package io.iohk.ethereum.network

import java.util.concurrent.atomic.AtomicLong

import io.micrometer.core.instrument.Counter

import io.iohk.ethereum.metrics.MetricsContainer

case object NetworkMetrics extends MetricsContainer {

  final private val HandshakedIncomingPeersGauge =
    metrics.registry.gauge("network.peers.incoming.handshaked.gauge", new AtomicLong(0))
  final private val HandshakedOutgoingPeersGauge =
    metrics.registry.gauge("network.peers.outgoing.handshaked.gauge", new AtomicLong(0))

  final val ReceivedMessagesCounter: Counter = metrics.counter("network.messages.received.counter")

  final val SentMessagesCounter: Counter = metrics.counter("network.messages.sent.counter")

  final val DiscoveredPeersSize: AtomicLong =
    metrics.registry.gauge("network.discovery.foundPeers.gauge", new AtomicLong(0))

  final val BlacklistedPeersSize: AtomicLong =
    metrics.registry.gauge("network.peers.blacklisted.gauge", new AtomicLong(0))

  final val BlacklistedReasonsFastSyncGroup: Counter =
    metrics.registry.counter("network.peers.blacklisted.fastSyncGroup.counter")
  final val BlacklistedReasonsRegularSyncGroup: Counter =
    metrics.registry.counter("network.peers.blacklisted.regularSyncGroup.counter")
  final val BlacklistedReasonsP2PGroup: Counter = metrics.registry.counter("network.peers.blacklisted.p2pGroup.counter")

  final val PendingPeersSize: AtomicLong = metrics.registry.gauge("network.peers.pending.gauge", new AtomicLong(0))

  final val TriedPeersSize: AtomicLong =
    metrics.registry.gauge("network.tried.peers.gauge", new AtomicLong(0L))

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
