package io.iohk.ethereum.vmrunner

import io.iohk.ethereum.vmrunner.ABI.Param

object ABI {
  case class Param(name: String, `type`: String)
}

case class ABI(`type`: String, name: String = "", inputs: Seq[Param] = Nil, outputs: Seq[Param] = Nil) {
  val shortSignature = s"$name(${inputs.map(_.`type`).mkString(",")})"

  val fullSignature = {
    val in = inputs.map(i => i.`type` + " " + i.name).mkString(", ")
    val out = outputs.map { o =>
      val s = if (o.name.isEmpty) "" else " "
      o.`type` + s + o.name
    }.mkString(", ")

    s"$name($in) returns ($out)"
  }
}
