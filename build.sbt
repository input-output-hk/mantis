enablePlugins(JDKPackagerPlugin, JavaAppPackaging, SolidityPlugin)

import scala.sys.process.Process

// Necessary for the nix build, please do not remove.
val nixBuild = sys.props.isDefinedAt("nix")

// Enable dev mode: disable certain flags, etc.
val mantisDev = sys.props.get("mantisDev").contains("true") || sys.env.get("MANTIS_DEV").contains("true")

val commonSettings = Seq(
  name := "mantis",
  version := "3.1.0",
  scalaVersion := "2.12.12",
  // Scalanet snapshots are published to Sonatype after each build.
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  testOptions in Test += Tests
    .Argument(TestFrameworks.ScalaTest, "-l", "EthashMinerSpec") // miner tests disabled by default
)

val malletDeps = Seq(
  Dependencies.scopt
).flatten ++ Seq(
  Dependencies.jline,
  Dependencies.jna
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
    Dependencies.network,
    Dependencies.twitterUtilCollection,
    Dependencies.crypto,
    Dependencies.scopt,
    Dependencies.logging,
    Dependencies.apacheCommons,
    Dependencies.micrometer,
    Dependencies.prometheus,
    Dependencies.cli,
    Dependencies.dependencies
  ).flatten ++ malletDeps
}

val Integration = config("it") extend Test

val Benchmark = config("benchmark") extend Test

val Evm = config("evm") extend Test

val Ets = config("ets") extend Test

val Snappy = config("snappy") extend Test

val Rpc = config("rpcTest") extend Test

val agent = project
  .settings(
    commonSettings,
    name := "agent",
    packageOptions in (Compile, packageBin) +=
      Package.ManifestAttributes( "Premain-Class" -> "agent.Agent" )
  )

val root = {
  val root = project
    .in(file("."))
    .configs(Integration, Benchmark, Evm, Ets, Snappy, Rpc)
    .enablePlugins(BuildInfoPlugin)
    .settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, git.gitHeadCommit),
      buildInfoPackage := "io.iohk.ethereum.utils",
      fork in Test := true,
      javaOptions in Test += ("-javaagent:" + (packageBin in (agent, Compile)).value)
    )
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= dep
    )
    .settings(executableScriptName := name.value)
    .settings(
      inConfig(Integration)(
        Defaults.testSettings
          ++ org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings :+ (Test / parallelExecution := false)
      ): _*
    )
    .settings(inConfig(Benchmark)(Defaults.testSettings :+ (Test / parallelExecution := false)): _*)
    .settings(inConfig(Evm)(Defaults.testSettings :+ (Test / parallelExecution := false)): _*)
    .settings(inConfig(Ets)(Defaults.testSettings :+ (Test / parallelExecution := false)): _*)
    .settings(inConfig(Snappy)(Defaults.testSettings :+ (Test / parallelExecution := false)): _*)
    .settings(inConfig(Rpc)(Defaults.testSettings :+ (Test / parallelExecution := false)): _*)
    .dependsOn(agent)

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
  "-Ypartial-unification",
  "-encoding",
  "utf-8"
)

scalacOptions in (Compile, console) ~= (_.filterNot(
  Set(
    "-Ywarn-unused-import",
    "-Xfatal-warnings"
  )
))

scalacOptions ~= (options => if (mantisDev) options.filterNot(_ == "-Xfatal-warnings") else options)

Test / parallelExecution := true

testOptions in Test += Tests.Argument("-oDG")

// protobuf compilation
// Into a subdirectory of src_managed to avoid it deleting other generated files; see https://github.com/sbt/sbt-buildinfo/issues/149
PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value / "protobuf"
)

// have the protobuf API version file as a resource
unmanagedResourceDirectories in Compile += baseDirectory.value / "src" / "main" / "protobuf"

(test in Evm) := (test in Evm).dependsOn(solidityCompile).value
(sourceDirectory in Evm) := baseDirectory.value / "src" / "evmTest"

(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"
scalastyleSources in Test ++= { (unmanagedSourceDirectories in Integration).value }

// Packaging
mainClass in Compile := Some("io.iohk.ethereum.App")
Universal / executableScriptName := name.value
discoveredMainClasses in Compile := Seq()
// Requires the 'ant-javafx.jar' that comes with Oracle JDK
// Enables creating an executable with the configuration files, has to be run on the OS corresponding to the desired version
ThisBuild / jdkPackagerType := "image"

Universal / mappings += (resourceDirectory in Compile).value / "logback.xml" -> "conf/logback.xml"

val sep = java.io.File.separator
jdkPackagerJVMArgs := Seq(
  "-Dconfig.file=." + sep + "conf" + sep + "app.conf",
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

// prepare PR
addCommandAlias(
  "pp",
  """;compile-all
    |;scalafmtAll
    |;scalastyle
    |;test:scalastyle
    |;testQuick
    |;it:test
    |""".stripMargin
)
