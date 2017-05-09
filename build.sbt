enablePlugins(JavaAppPackaging, SolidityPlugin)

val commonSettings = Seq(
  name := "etc-client",
  version := "0.1",
  scalaVersion := "2.12.1"
)

val dep = {
  val akkaVersion = "2.4.17"
  val akkaHttpVersion = "10.0.6"
  val circeVersion = "0.7.0"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-agent" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
    "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
    "org.json4s" %% "json4s-native" % "3.5.1",
    "de.heikoseeberger" %% "akka-http-json4s" % "1.11.0",
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "it,test",
    "io.suzaku" %% "boopickle" % "1.2.6",
    "org.consensusresearch" %% "scrypto" % "1.2.0-RC3",
    "com.madgag.spongycastle" % "core" % "1.54.0.0",
    "org.iq80.leveldb" % "leveldb" % "0.9",
    "org.scorexfoundation" %% "iodb" % "0.3.0",
    "ch.qos.logback" % "logback-classic" % "1.1.9",
    "org.scalatest" %% "scalatest" % "3.0.1" % "it,test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "it,test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "it,test",
    "ch.qos.logback" % "logback-classic" % "1.1.9",
    "org.jline" % "jline" % "3.1.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
    "com.miguno.akka" %% "akka-mock-scheduler" % "0.5.1" % "it,test"
  )
}

val Integration = config("it") extend Test

val Evm = config("evm") extend Test

val root = project.in(file("."))
    .configs(Integration)
    .configs(Evm)
    .settings(commonSettings: _*)
    .settings(libraryDependencies ++= dep)
    .settings(inConfig(Integration)(Defaults.testSettings) : _*)
    .settings(inConfig(Evm)(Defaults.testSettings) : _*)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings"
)

testOptions in Test += Tests.Argument("-oD")

(test in Evm) := (test in Evm).dependsOn(solidityCompile).value
(sourceDirectory in Evm) := baseDirectory.value / "src" / "evmTest"

(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"
scalastyleSources in Test ++= {(unmanagedSourceDirectories in Integration).value}

mainClass in Compile := Some("io.iohk.ethereum.App")

coverageExcludedPackages := "io.iohk.ethereum.vmrunner.*"
