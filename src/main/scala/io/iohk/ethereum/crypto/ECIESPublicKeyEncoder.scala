package io.iohk.ethereum.crypto

import org.spongycastle.crypto.KeyEncoder
import org.spongycastle.crypto.params.AsymmetricKeyParameter
import org.spongycastle.crypto.params.ECPublicKeyParameters

/**
  * Created by Anton Nashatyrev on 01.10.2015.
  */
class ECIESPublicKeyEncoder extends KeyEncoder {
  override def getEncoded(keyParameter: AsymmetricKeyParameter): Array[Byte] =
    keyParameter.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)
}
