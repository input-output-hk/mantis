import java.io.PrintWriter

enablePlugins(JavaAppPackaging)

val commonSettings = Seq(
  name := "etc-client",
  version := "0.1",
  scalaVersion := "2.12.1"
)

val dep = {
  val akkaVersion = "2.4.17"
  val akkaHttpVersion = "10.0.3"
  val circeVersion = "0.7.0"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-agent" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
    "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
    "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "it,test",
    "io.suzaku" %% "boopickle" % "1.2.6",
    "org.consensusresearch" %% "scrypto" % "1.2.0-RC3",
    "com.madgag.spongycastle" % "core" % "1.54.0.0",
    "org.iq80.leveldb" % "leveldb" % "0.9",
    "org.scorexfoundation" %% "iodb" % "0.2.0",
    "ch.qos.logback" % "logback-classic" % "1.1.9",
    "org.scalatest" %% "scalatest" % "3.0.1" % "it,test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "it,test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "it,test",
    "org.scorexfoundation" %% "iodb" % "0.2.0",
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

lazy val solidityCompileTask = TaskKey[Unit]("solidityCompile", "Compiles solidity contracts")

solidityCompileTask := {
  import sys.process._

  val contractsDir = baseDirectory.value / "src" / "evmTest" / "resources" / "solidity"
  val outDir = baseDirectory.value / "target" / "contracts"

  (contractsDir ** "*.sol").get.foreach { f =>
    Seq("solc", f.getPath, "--bin", "-o", outDir.getPath).!!

    // this is a temporary workaround, see: https://github.com/ethereum/solidity/issues/1732
    val abiOut = Seq("solc", f.getPath, "--abi").!!
    val abisLines = abiOut.split("\n").sliding(4, 4)
    abisLines.foreach { abiLines =>
      val contractName = abiLines(1)
        .replace(f.getPath, "")
        .dropWhile(_ != ':').drop(1)
        .takeWhile(_ != ' ')
      new PrintWriter(outDir / s"$contractName.abi") {
        write(abiLines.drop(3).mkString); close()
      }
    }
  }
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

(test in Evm) := (test in Evm).dependsOn(solidityCompileTask).value
(sourceDirectory in Evm) := baseDirectory.value / "src" / "evmTest"

(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"
scalastyleSources in Test ++= {(unmanagedSourceDirectories in Integration).value}
