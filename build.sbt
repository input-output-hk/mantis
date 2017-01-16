name := "etc-client"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "scorex-core" % "2.0.0-M3-feature-handshake",
  "com.madgag.spongycastle" % "core" % "1.53.0.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scorexfoundation" %% "iodb" % "0.1.1"
)