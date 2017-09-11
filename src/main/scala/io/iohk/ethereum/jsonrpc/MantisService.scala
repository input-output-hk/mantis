package io.iohk.ethereum.jsonrpc

import java.text.Normalizer
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.MantisService.ImportMnemonicRequest
import io.iohk.ethereum.jsonrpc.PersonalService.{ImportRawKeyRequest, ImportRawKeyResponse}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object MantisService {

  case class ImportMnemonicRequest(mnemonic: String, passphrase: String)

  val Pbkdf2Algorithm = "PBKDF2WithHmacSHA512"
  val Pbkdf2Iterations = 2048
  val Pbkdf2KeyLength = PersonalService.PrivateKeyLength * 8

}

class MantisService(
  personalService: PersonalService
) {

  /**
    * Imports an account into personal wallets using an algorithm as the one stated in BIP-39 specification but generating
    * io.iohk.ethereum.jsonrpc.PersonalService.PrivateKeyLength
    * (https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#Generating_the_mnemonic)
    */
  def importMnemonic(req: ImportMnemonicRequest): ServiceResponse[ImportRawKeyResponse] = {
    import MantisService._

    Future {
      val normalizedMnemonic = Normalizer.normalize(req.mnemonic, Normalizer.Form.NFKD).toCharArray
      val normalizedSeed = Normalizer.normalize(s"mnemonic${req.passphrase}", Normalizer.Form.NFKD)

      val spec = new PBEKeySpec(
        normalizedMnemonic,
        normalizedSeed.getBytes,
        Pbkdf2Iterations,
        Pbkdf2KeyLength
      )
      val skf = SecretKeyFactory.getInstance(Pbkdf2Algorithm)

      ByteString(skf.generateSecret(spec).getEncoded)
    }.flatMap { prvKey =>
      val importRawKeyRequest = ImportRawKeyRequest(prvKey, req.passphrase)
      personalService.importRawKey(importRawKeyRequest)
    }
  }

}
