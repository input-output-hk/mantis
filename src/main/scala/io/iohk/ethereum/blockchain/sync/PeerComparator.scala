package io.iohk.ethereum.blockchain

import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo

object PeerComparator {

  def doPeersHaveSameBestBlock(peerInfo1: PeerInfo, peerInfo2: PeerInfo): Boolean =
    peerInfo1.bestBlockHash == peerInfo2.bestBlockHash
}
