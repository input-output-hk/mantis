package io.iohk.ethereum.consensus
package ethash.validators

import io.iohk.ethereum.consensus.validators.Validators

trait EthashValidators extends Validators {
  def ommersValidator: OmmersValidator
}
