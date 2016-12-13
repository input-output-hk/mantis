name := "etc-client"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "scorex-core" % "2.0.0-M2",
  "com.madgag.spongycastle" % "core" % "1.54.0.0",

  "org.scalatest" %% "scalatest" % "2.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.+" % "test"
)