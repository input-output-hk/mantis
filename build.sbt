enablePlugins(JDKPackagerPlugin, JavaAppPackaging, SolidityPlugin, JavaAgent)

javaAgents += "io.kamon" % "kanela-agent" % "1.0.6"

import scala.sys.process.Process
import NativePackagerHelper._
import com.typesafe.sbt.SbtGit.GitKeys._

// Necessary for the nix build, please do not remove.
val nixBuild = sys.props.isDefinedAt("nix")

// Enable dev mode: disable certain flags, etc.
val mantisDev = sys.props.get("mantisDev").contains("true") || sys.env.get("MANTIS_DEV").contains("true")

lazy val compilerOptimizationsForProd = Seq(
  "-opt:l:method", // method-local optimizations
  "-opt:l:inline", // inlining optimizations
  "-opt-inline-from:io.iohk.**" // inlining the project only
)

// Releasing. https://github.com/olafurpg/sbt-ci-release
inThisBuild(
  List(
    organization := "io.iohk",
    homepage := Some(url("https://github.com/input-output-hk/mantis")),
    scmInfo := Some(
      ScmInfo(url("https://github.com/input-output-hk/mantis"), "git@github.com:input-output-hk/mantis.git")
    ),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List()
  )
)

// https://github.com/sbt/sbt/issues/3570
updateOptions := updateOptions.value.withGigahorse(false)

// artifact name will include scala version
crossPaths := true

// patch for error on 'early-semver' problems
ThisBuild / evictionErrorLevel := Level.Info

val `scala-2.12` = "2.12.13"
val `scala-2.13` = "2.13.6"
val supportedScalaVersions = List(`scala-2.12`, `scala-2.13`)

def commonSettings(projectName: String): Seq[sbt.Def.Setting[_]] = Seq(
  name := projectName,
  organization := "io.iohk",
  scalaVersion := `scala-2.13`,
  semanticdbEnabled := true, // enable SemanticDB
  semanticdbVersion := scalafixSemanticdb.revision, // use Scalafix compatible version
  ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value),
  ThisBuild / scalafixDependencies ++= List(
    "com.github.liancheng" %% "organize-imports" % "0.5.0",
    "com.github.vovapolu" %% "scaluzzi" % "0.1.16"
  ),
  // Scalanet snapshots are published to Sonatype after each build.
  resolvers += "Sonatype OSS Snapshots".at("https://oss.sonatype.org/content/repositories/snapshots"),
  (Test / testOptions) += Tests
    .Argument(TestFrameworks.ScalaTest, "-l", "EthashMinerSpec"), // miner tests disabled by default,
  scalacOptions := Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    // https://www.scala-lang.org/2021/01/12/configuring-and-suppressing-warnings.html
    // cat={warning-name}:ws prints a summary with the number of warnings of the given type
    // any:e turns all remaining warnings into errors
    if (sys.env.get("MANTIS_FULL_WARNS").contains("true") || nixBuild) {
      "-Wconf:any:w"
    }
    else {
      "-Wconf:cat=deprecation:ws,cat=lint-package-object-classes:ws,cat=unused:ws,cat=lint-infer-any:ws,cat=lint-byname-implicit:ws,cat=other-match-analysis:ws,any:e"
    } ,
    "-Ywarn-unused",
    "-Xlint",
    "-encoding",
    "utf-8"
  ) ++ Seq("-Ypatmat-exhaust-depth", "off"),
  scalacOptions ++= (if (mantisDev) Seq.empty else compilerOptimizationsForProd),
  (Compile / console / scalacOptions) ~= (_.filterNot(
    Set(
      "-Ywarn-unused-import",
      "-Xfatal-warnings"
    )
  )),
  scalacOptions ~= (options => if (mantisDev) options.filterNot(_ == "-Xfatal-warnings") else options),
  Test / parallelExecution := true,
  (Test / testOptions) += Tests.Argument("-oDG"),
  (Test / scalastyleConfig) := file("scalastyle-test-config.xml"),
  // Only publish selected libraries.
  (publish / skip) := true
)

val publishSettings = Seq(
  publish / skip := false,
  crossScalaVersions := supportedScalaVersions
)

// Adding an "it" config because in `Dependencies.scala` some are declared with `% "it,test"`
// which would fail if the project didn't have configuration to add to.
val Integration = config("it").extend(Test)

lazy val bytes = {
  val bytes = project
    .in(file("bytes"))
    .configs(Integration)
    .settings(commonSettings("mantis-bytes"))
    .settings(inConfig(Integration)(scalafixConfigSettings(Integration)))
    .settings(publishSettings)
    .settings(
      libraryDependencies ++=
        Dependencies.akkaUtil ++
          Dependencies.testing
    )

  bytes
}

lazy val crypto = {
  val crypto = project
    .in(file("crypto"))
    .configs(Integration)
    .dependsOn(bytes)
    .settings(commonSettings("mantis-crypto"))
    .settings(inConfig(Integration)(scalafixConfigSettings(Integration)))
    .settings(publishSettings)
    .settings(
      libraryDependencies ++=
        Dependencies.akkaUtil ++
          Dependencies.crypto ++
          Dependencies.testing
    )

  crypto
}

lazy val rlp = {
  val rlp = project
    .in(file("rlp"))
    .configs(Integration)
    .dependsOn(bytes)
    .settings(commonSettings("mantis-rlp"))
    .settings(inConfig(Integration)(scalafixConfigSettings(Integration)))
    .settings(publishSettings)
    .settings(
      libraryDependencies ++=
        Dependencies.akkaUtil ++
          Dependencies.shapeless ++
          Dependencies.testing
    )

  rlp
}

lazy val node = {
  val Benchmark = config("benchmark").extend(Test)

  val Evm = config("evm").extend(Test)

  val Rpc = config("rpcTest").extend(Test)

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
      Dependencies.apacheCommons,
      Dependencies.boopickle,
      Dependencies.cats,
      Dependencies.circe,
      Dependencies.cli,
      Dependencies.crypto,
      Dependencies.dependencies,
      Dependencies.enumeratum,
      Dependencies.guava,
      Dependencies.json4s,
      Dependencies.kamon,
      Dependencies.logging,
      Dependencies.micrometer,
      Dependencies.monix,
      Dependencies.network,
      Dependencies.prometheus,
      Dependencies.rocksDb,
      Dependencies.scaffeine,
      Dependencies.scopt,
      Dependencies.testing
    ).flatten ++ malletDeps
  }

  (Test / scalastyleSources) ++= (Integration / unmanagedSourceDirectories).value

  (Evm / test) := (Evm / test).dependsOn(solidityCompile).value
  (Evm / sourceDirectory) := baseDirectory.value / "src" / "evmTest"

  val node = project
    .in(file("."))
    .configs(Integration, Benchmark, Evm, Rpc)
    .enablePlugins(BuildInfoPlugin)
    .dependsOn(bytes, crypto, rlp)
    .settings(
      buildInfoKeys := BuildInfoKey.ofN(
        name,
        version,
        scalaVersion,
        sbtVersion,
        gitHeadCommit,
        gitCurrentBranch,
        gitCurrentTags,
        gitDescribedVersion,
        gitUncommittedChanges,
        (Compile / libraryDependencies)
      ),
      buildInfoPackage := "io.iohk.ethereum.utils",
      (Test / fork) := true,
      (Compile / buildInfoOptions) += BuildInfoOption.ToMap
    )
    .settings(commonSettings("mantis"): _*)
    .settings(inConfig(Integration)(scalafixConfigSettings(Integration)))
    .settings(inConfig(Evm)(scalafixConfigSettings(Evm)))
    .settings(inConfig(Rpc)(scalafixConfigSettings(Rpc)))
    .settings(
      libraryDependencies ++= dep
    )
    .settings(
      executableScriptName := name.value
    )
    .settings(
      inConfig(Integration)(
        Defaults.testSettings
          ++ org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings :+ (Test / parallelExecution := false)
      ): _*
    )
    .settings(inConfig(Benchmark)(Defaults.testSettings :+ (Test / parallelExecution := false)): _*)
    .settings(inConfig(Evm)(Defaults.testSettings :+ (Test / parallelExecution := false)): _*)
    .settings(inConfig(Rpc)(Defaults.testSettings :+ (Test / parallelExecution := false)): _*)
    .settings(
      // protobuf compilation
      // Into a subdirectory of src_managed to avoid it deleting other generated files; see https://github.com/sbt/sbt-buildinfo/issues/149
      (Compile / PB.targets) := Seq(
        scalapb.gen() -> (Compile / sourceManaged).value / "protobuf"
      ),
      // have the protobuf API version file as a resource
      (Compile / unmanagedResourceDirectories) += baseDirectory.value / "src" / "main" / "protobuf",
      // Packaging
      (Compile / mainClass) := Some("io.iohk.ethereum.App"),
      (Compile / discoveredMainClasses) := Seq(),
      // Requires the 'ant-javafx.jar' that comes with Oracle JDK
      // Enables creating an executable with the configuration files, has to be run on the OS corresponding to the desired version
      ThisBuild / jdkPackagerType := "image",
      (Universal / mappings) ++= directory((Compile / resourceDirectory).value / "conf"),
      (Universal / mappings) += (Compile / resourceDirectory).value / "logback.xml" -> "conf/logback.xml",
      bashScriptExtraDefines += """addJava "-Dconfig.file=${app_home}/../conf/app.conf"""",
      bashScriptExtraDefines += """addJava "-Dlogback.configurationFile=${app_home}/../conf/logback.xml"""",
      batScriptExtraDefines += """call :add_java "-Dconfig.file=%APP_HOME%\conf\app.conf"""",
      batScriptExtraDefines += """call :add_java "-Dlogback.configurationFile=%APP_HOME%\conf\logback.xml""""
    )
    .settings(
      crossScalaVersions := List(`scala-2.13`)
    )

  if (!nixBuild)
    node
  else
    //node.settings(PB.protocExecutable := file("protoc"))
    node.settings((Compile / PB.runProtoc) := (args => Process("protoc", args) !))

}

coverageExcludedPackages := "io\\.iohk\\.ethereum\\.extvm\\.msg.*"

addCommandAlias(
  "compile-all",
  """;bytes/compile
    |;bytes/test:compile
    |;crypto/compile
    |;crypto/test:compile
    |;rlp/compile
    |;rlp/test:compile
    |;compile
    |;test:compile
    |;evm:compile
    |;it:compile
    |;rpcTest:compile
    |;benchmark:compile
    |""".stripMargin
)

// prepare PR
addCommandAlias(
  "pp",
  """;compile-all
    |;bytes/scalafmtAll
    |;bytes/scalastyle
    |;bytes/test:scalastyle
    |;crypto/scalafmtAll
    |;crypto/scalastyle
    |;crypto/test:scalastyle
    |;rlp/scalafmtAll
    |;rlp/scalastyle
    |;rlp/test:scalastyle
    |;scalafmtAll
    |;scalastyle
    |;test:scalastyle
    |;rlp/test
    |;testQuick
    |;it:test
    |""".stripMargin
)

// format all modules
addCommandAlias(
  "formatAll",
  """;compile-all
    |;bytes/scalafixAll
    |;bytes/scalafmtAll
    |;crypto/scalafixAll
    |;crypto/scalafmtAll
    |;rlp/scalafixAll
    |;rlp/scalafmtAll
    |;scalafixAll
    |;scalafmtAll
    |""".stripMargin
)

// check modules formatting
addCommandAlias(
  "formatCheck",
  """;compile-all
    |;bytes/scalafixAll --check
    |;bytes/scalafmtCheckAll
    |;crypto/scalafixAll --check
    |;crypto/scalafmtCheckAll
    |;rlp/scalafixAll --check
    |;rlp/scalafmtCheckAll
    |;scalafixAll --check
    |;scalafmtCheckAll
    |""".stripMargin
)

// testAll
addCommandAlias(
  "testAll",
  """;compile-all
    |;rlp/test
    |;bytes/test
    |;crypto/test
    |;test
    |;it:test
    |""".stripMargin
)

(ThisBuild / scapegoatVersion) := "1.4.9"
scapegoatReports := Seq("xml")
