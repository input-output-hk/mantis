package io.iohk.ethereum.domain

/** An individual node used to prove a path down a merkle-patricia-tree */
final case class ProofNode[V](b: V) extends AnyVal
