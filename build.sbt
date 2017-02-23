val commonSettings = Seq(
  name := "etc-client",
  version := "0.1",
  scalaVersion := "2.12.1"
)

val dep = {
  val akkaVersion = "2.4.17"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-agent" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
    "org.consensusresearch" %% "scrypto" % "1.2.0-RC3",
    "com.madgag.spongycastle" % "core" % "1.54.0.0",
    "org.iq80.leveldb" % "leveldb" % "0.9",
    "org.scorexfoundation" %% "iodb" % "0.2.0",
    "ch.qos.logback" % "logback-classic" % "1.1.9",
    "org.scalatest" %% "scalatest" % "3.0.1" % "it,test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "it,test",
    "com.miguno.akka" %% "akka-mock-scheduler" % "0.5.1" % "it,test"
  )
}

val Integration = config("it") extend Test

val root = project.in(file("."))
    .configs(Integration)
    .settings(commonSettings: _*)
    .settings(libraryDependencies ++= dep)
    .settings(inConfig(Integration)(Defaults.testSettings) : _*)

scalacOptions := Seq("-unchecked", "-deprecation", "-feature")

(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"
scalastyleSources in Test ++= {(unmanagedSourceDirectories in Integration).value}