val commonSettings = Seq(
  name := "etc-client",
  version := "0.1",
  scalaVersion := "2.11.8"
)

val dep = {
  val akkaVersion = "2.4.16"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
    "org.consensusresearch" %% "scrypto" % "1.2.0-RC3",
    "com.madgag.spongycastle" % "core" % "1.54.0.0",
    "org.scalatest" %% "scalatest" % "3.0.1" % "it,test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "it,test",
    "org.scorexfoundation" %% "iodb" % "0.1.1",
    "ch.qos.logback" % "logback-classic" % "1.1.9"
  )
}

val Integration = config("it") extend Test

val root = project.in(file("."))
    .configs(Integration)
    .settings(commonSettings: _*)
    .settings(libraryDependencies ++= dep)
    .settings(inConfig(Integration)(Defaults.testSettings) : _*)

scalacOptions := Seq("-unchecked", "-deprecation")
