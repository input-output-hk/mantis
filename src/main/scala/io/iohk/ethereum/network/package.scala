package io.iohk.ethereum

import org.spongycastle.asn1.sec.SECNamedCurves
import org.spongycastle.crypto.params.ECDomainParameters

package object network {
  val curveParams = SECNamedCurves.getByName("secp256k1")
  val curve = new ECDomainParameters(curveParams.getCurve, curveParams.getG, curveParams.getN, curveParams.getH)
}
