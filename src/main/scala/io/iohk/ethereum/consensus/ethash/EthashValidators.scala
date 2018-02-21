package io.iohk.ethereum
package consensus
package ethash

import io.iohk.ethereum.validators.OmmersValidator

trait EthashValidators extends consensus.Validators {
  def ommersValidator: OmmersValidator

}
