name := "etc-client"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.16",
  "org.consensusresearch" %% "scrypto" % "1.2.0-RC3",
  "com.madgag.spongycastle" % "core" % "1.54.0.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test")

