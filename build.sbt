enablePlugins(JDKPackagerPlugin, JavaAppPackaging, SolidityPlugin)

val commonSettings = Seq(
  name := "mantis",
  version := "3.0",
  scalaVersion := "2.12.12",
  testOptions in Test += Tests
    .Argument(TestFrameworks.ScalaTest, "-l", "EthashMinerSpec") // miner tests disabled by default
)

// Resolver for rocksDb
resolvers += "rocksDb" at "https://dl.bintray.com/ethereum/maven/"

val malletDeps = Seq(
  Dependencies.scopt
).flatten ++ Seq(
  "org.jline" % "jline" % "3.1.2",
  "net.java.dev.jna" % "jna" % "4.5.1"
)

val dep = {
  Seq(
    Dependencies.akka,
    Dependencies.akkaHttp,
    Dependencies.json4s,
    Dependencies.circe,
    Dependencies.boopickle,
    Dependencies.rocksDb,
    Dependencies.enumeratum,
    Dependencies.testing,
    Dependencies.cats,
    Dependencies.monix,
    Dependencies.twitterUtilCollection,
    Dependencies.crypto,
    Dependencies.scopt,
    Dependencies.logging,
    Dependencies.apacheCommons,
    Dependencies.metrics,
    Dependencies.dependencies
  ).flatten ++ malletDeps
}

val Integration = config("it") extend Test

val Benchmark = config("benchmark") extend Test

val Evm = config("evm") extend Test

val Ets = config("ets") extend Test

val Snappy = config("snappy") extend Test

val Rpc = config("rpcTest") extend Test

val root = project
  .in(file("."))
  .configs(Integration, Benchmark, Evm, Ets, Snappy, Rpc)
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= dep,
    executableScriptName := name.value
  )
  .settings(inConfig(Integration)(Defaults.testSettings): _*)
  .settings(inConfig(Benchmark)(Defaults.testSettings): _*)
  .settings(inConfig(Evm)(Defaults.testSettings): _*)
  .settings(inConfig(Ets)(Defaults.testSettings): _*)
  .settings(inConfig(Snappy)(Defaults.testSettings): _*)
  .settings(inConfig(Rpc)(Defaults.testSettings): _*)

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
