package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.domain.{Account, Address, UInt256}
import io.iohk.ethereum.jsonrpc.ProofService.{GetProofRequest, GetProofResponse, ProofAccount}
import monix.eval.Task

object ProofServiceDummy extends ProofService {

  val EmptyAddress: Address = Address(Account.EmptyCodeHash)
  val EmptyProofAccount: ProofAccount = ProofAccount(
    EmptyAddress,
    Seq.empty,
    BigInt(42),
    Account.EmptyCodeHash,
    UInt256.Zero,
    Account.EmptyStorageRootHash,
    Seq.empty
  )
  val EmptyProofResponse: GetProofResponse = GetProofResponse(EmptyProofAccount)

  override def getProof(req: GetProofRequest): ServiceResponse[GetProofResponse] = {
    Task.now(Right(EmptyProofResponse))
  }
}
