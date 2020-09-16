enablePlugins(JDKPackagerPlugin, JavaAppPackaging, SolidityPlugin)

import scala.sys.process.Process

// Necessary for the nix build, please do not remove.
val nixBuild = sys.props.isDefinedAt("nix")

val commonSettings = Seq(
  name := "mantis-core",
  version := "3.0",
  scalaVersion := "2.12.12",
  testOptions in Test += Tests
    .Argument(TestFrameworks.ScalaTest, "-l", "EthashMinerSpec") // miner tests disabled by default
)

// Resolver for rocksDb
resolvers += "rocksDb" at "https://dl.bintray.com/ethereum/maven/"

val dep = {
  val akkaVersion = "2.6.9"
  val akkaHttpVersion = "10.2.0"
  val circeVersion = "0.9.3"
  val rocksDb = "5.9.2"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
    "com.typesafe.akka" %% "akka-stream" % akkaVersion,
    "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
    "ch.megard" %% "akka-http-cors" % "1.1.0",
    "org.json4s" %% "json4s-native" % "3.5.4",
    "de.heikoseeberger" %% "akka-http-json4s" % "1.34.0",
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "it,test",
    "io.suzaku" %% "boopickle" % "1.3.0",
    "org.ethereum" % "rocksdbjni" % rocksDb,
    "org.scalatest" %% "scalatest" % "3.0.5" % "it,test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.0" % "it,test",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "org.jline" % "jline" % "3.1.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
    "com.miguno.akka" %% "akka-mock-scheduler" % "0.5.5" % "it,test",
    "commons-io" % "commons-io" % "2.6",
    "org.scala-sbt.ipcsocket" % "ipcsocket" % "1.0.0",
    "org.bouncycastle" % "bcprov-jdk15on" % "1.59",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
    "org.typelevel" %% "mouse" % "0.23",
    "org.typelevel" %% "cats-core" % "2.0.0",
    "org.typelevel" %% "cats-effect" % "2.0.0",
    "com.twitter" %% "util-collection" % "18.5.0",
    "com.google.guava" % "guava" % "28.0-jre",
    "io.monix" %% "monix" % "3.1.0",
    "com.beachape" %% "enumeratum" % "1.5.13",
    "com.beachape" %% "enumeratum-cats" % "1.5.15",
    "com.beachape" %% "enumeratum-scalacheck" % "1.5.16" % Test,
    // mallet deps
    "org.jline" % "jline" % "3.1.2",
    "net.java.dev.jna" % "jna" % "4.5.1",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
    "com.github.scopt" %% "scopt" % "3.7.0",
    // Metrics (https://github.com/DataDog/java-dogstatsd-client)
    "com.datadoghq" % "java-dogstatsd-client" % "2.5",
    "org.xerial.snappy" % "snappy-java" % "1.1.7.2",
    "org.web3j" % "core" % "3.4.0" % "test"
  )
}

val Integration = config("it") extend Test

val Benchmark = config("benchmark") extend Test

val Evm = config("evm") extend Test

val Ets = config("ets") extend Test

val Snappy = config("snappy") extend Test

val Rpc = config("rpcTest") extend Test

val root = {
  val root = project.in(file("."))
    .configs(Integration, Benchmark, Evm, Ets, Snappy, Rpc)
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= dep,
      executableScriptName := name.value
    )
    .settings(inConfig(Integration)(Defaults.testSettings) : _*)
    .settings(inConfig(Benchmark)(Defaults.testSettings) : _*)
    .settings(inConfig(Evm)(Defaults.testSettings) : _*)
    .settings(inConfig(Ets)(Defaults.testSettings) : _*)
    .settings(inConfig(Snappy)(Defaults.testSettings) : _*)
    .settings(inConfig(Rpc)(Defaults.testSettings) : _*)

  if (!nixBuild)
    root
  else
    root.settings(PB.runProtoc in Compile := (args => Process("protoc", args) !))
}

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-Xlint:unsound-match",
  "-Ywarn-inaccessible",
  "-Ywarn-unused-import",
  "-encoding",
  "utf-8"
)

scalacOptions in (Compile, console) ~= (_.filterNot(
  Set(
    "-Ywarn-unused-import",
    "-Xfatal-warnings"
  )
))

parallelExecution in Test := false

testOptions in Test += Tests.Argument("-oDG")

// protobuf compilation
PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)

// have the protobuf API version file as a resource
unmanagedResourceDirectories in Compile += baseDirectory.value / "src" / "main" / "protobuf"

(test in Evm) := (test in Evm).dependsOn(solidityCompile).value
(sourceDirectory in Evm) := baseDirectory.value / "src" / "evmTest"

(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"
scalastyleSources in Test ++= { (unmanagedSourceDirectories in Integration).value }

mainClass in Compile := Some("io.iohk.ethereum.App")

// Requires the 'ant-javafx.jar' that comes with Oracle JDK
// Enables creating an executable with the configuration files, has to be run on the OS corresponding to the desired version
jdkPackagerType := "image"

val sep = java.io.File.separator
jdkPackagerJVMArgs := Seq(
  "-Dconfig.file=." + sep + "conf" + sep + "mantis.conf",
  "-Dlogback.configurationFile=." + sep + "conf" + sep + "logback.xml",
  "-Xss10M"
)

coverageExcludedPackages := "io\\.iohk\\.ethereum\\.extvm\\.msg.*"


addCommandAlias(
  "compile-all",
  """;compile
    |;test:compile
    |;evm:compile
    |;it:compile
    |;ets:compile
    |;rpcTest:compile
    |;snappy:compile
    |;benchmark:compile
    |""".stripMargin
)
