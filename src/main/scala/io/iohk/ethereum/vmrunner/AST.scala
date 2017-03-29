package io.iohk.ethereum.vmrunner

object AST {
  sealed trait Cmd

  case class Create(name: String, attrs: Seq[Attr]) extends Cmd
  case object Accounts extends Cmd
  case object DoNothing extends Cmd

  case class Object(name: String) extends Cmd
  case class FunCall(target: String, name: String, args: Seq[String], attrs: Seq[Attr]) extends Cmd
  case class Property(target: String, name: String) extends Cmd

  case class Attr(name: String, value: BigInt)
}
