package io.iohk.ethereum
package consensus
package ethash

import io.iohk.ethereum.consensus.validators.{OmmersValidator, Validators}

trait EthashValidators extends Validators {
  def ommersValidator: OmmersValidator
}
