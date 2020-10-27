package io.iohk.ethereum.forking
import akka.http.scaladsl.model.Uri
import TxGenerator.Account

object Config {
  val clientsMap = Map(
    "mantis-1" -> Uri("http://localhost:8545"),
    "mantis-2" -> Uri("http://localhost:8546"),
    "mantis-3" -> Uri("http://localhost:8547"),
    "mantis-4" -> Uri("http://localhost:8548"),
    "mantis-5" -> Uri("http://localhost:8549")
  )

  val accounts = List(
    Account(
      "765f4cf5d31643c6df8854b1e611c51c939b94bb41ae3a14d3213d0e4557d415",
      "331e0c6ea195c813c53ad75b5d8cdf2110d17950",
      "Zaq12wsx"
    ),
    Account(
      "6c89beabb4d8d38115d9c21aa0e066464452849f1c36a57a3d6c18bfb3fb607d",
      "19502f966075d4314c1abc27d2bb6d689197fd01",
      "Zaq12wsx"
    ),
    Account(
      "37110e032b00b631f38f5b1cbd2f32e7f9d419e7fc424cf0057713933f7e9d86",
      "61f3166be668120befbb11b6fd8a355b747a565c",
      "Zaq12wsx"
    ),
    Account(
      "2fdc158d79ba79b0429f451e8130edcd4a18681af3caaeee0a97236f526012b3",
      "6a101366423b39ecabd1d4977cedc28c87ff8a12",
      "Zaq12wsx"
    ),
    Account(
      "42eb3d6720ee72d2b48ca755860c7092cf861bdf7e14d588d8fef6c07b9f2e63",
      "6d42fea571b61102636e3d4911caa04374c4110d",
      "Zaq12wsx"
    )
  )

  val allReceivers = accounts.map(_.address)
}
