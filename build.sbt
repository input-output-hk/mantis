enablePlugins(JDKPackagerPlugin, JavaAppPackaging, SolidityPlugin, BuildInfoPlugin, GitVersioning)

import com.typesafe.sbt.SbtGit.GitKeys._

val commonSettings = Seq(
  name := "mantis",
  version := "1.0-daedalus-rc1",
  scalaVersion := "2.12.5",
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-l", "EthashMinerSpec") // miner tests disabled by default
)

// Temp resolver for LevelDB fork
resolvers += "stepsoft" at "http://nexus.mcsherrylabs.com/repository/releases/"
resolvers += "Clojars" at "http://clojars.org/repo"

val dep = {
  val akkaVersion = "2.4.17"
  val akkaHttpVersion = "10.0.6"
  val circeVersion = "0.7.0"

  val atomixVersion = "2.1.0-beta1"

  // Metrics
  val micrometerVersion = "1.0.4"
  val reactorCoreVersion = "3.1.4.RELEASE"
  val reactorNettyVersion = "0.7.4.RELEASE"

  // Logging
  val riemannVersion = "0.5.0"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-agent" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
    "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
    "ch.megard" %% "akka-http-cors" % "0.2.1",
    "org.json4s" %% "json4s-native" % "3.5.1",
    "de.heikoseeberger" %% "akka-http-json4s" % "1.11.0",
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "it,test",
    "io.suzaku" %% "boopickle" % "1.2.6",
    "org.consensusresearch" %% "scrypto" % "1.2.0-RC3",
    "com.madgag.spongycastle" % "core" % "1.56.0.0",
    "org.iq80.leveldb" % "leveldb" % "0.12",
    "org.iq80.leveldb" % "leveldb-api" % "0.12",
    "org.scorexfoundation" %% "iodb" % "0.3.0",
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
    "com.miguno.akka" %% "akka-mock-scheduler" % "0.5.1" % "it,test",
    "commons-io" % "commons-io" % "2.5",
    "com.typesafe.akka" %% "akka-stream" % akkaVersion,
    "org.scala-sbt.ipcsocket" % "ipcsocket" % "1.0.0",
    "com.twitter" %% "util-collection" % "18.4.0",
    "com.googlecode.protobuf-java-format" % "protobuf-java-format" % "1.4",

    // Pluggable Consensus: AtomixRaft
    "io.atomix" % "atomix" % atomixVersion,
    "io.atomix" % "atomix-raft" % atomixVersion,
    "io.netty" % "netty-tcnative-boringssl-static" % "2.0.7.Final" classifier "linux-x86_64", // using native epoll

    // mallet deps
    "org.jline" % "jline" % "3.1.2",
    "net.java.dev.jna" % "jna" % "4.5.1",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
    "com.github.scopt" %% "scopt" % "3.7.0",

    // Metrics (generic support)
    "io.micrometer" % "micrometer-core" % micrometerVersion,
    // Metrics (JMX export)
    "io.micrometer" % "micrometer-registry-jmx" % micrometerVersion,
    // Metrics (Datadog export)
    "io.micrometer" % "micrometer-registry-statsd" % micrometerVersion,
    "io.projectreactor" % "reactor-core" % reactorCoreVersion,
    "io.projectreactor.ipc" % "reactor-netty" % reactorNettyVersion,
    // Logging
    "io.riemann" % "riemann-java-client" % riemannVersion
  )
}

val Integration = config("it") extend Test

val Benchmark = config("benchmark") extend Test

val Evm = config("evm") extend Test

val Ets = config("ets") extend Test

val Snappy = config("snappy") extend Test

val root = project.in(file("."))
    .configs(Integration, Benchmark, Evm, Ets, Snappy)
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= dep,
      verifyOutputFile in verifyGenerate := baseDirectory.value / "verify.sbt",
      verifyOptions in verify := VerifyOptions(
        includeBin = true,
        includeScala = true,
        includeDependency = true,
        excludedJars = Nil,
        warnOnUnverifiedFiles = false,
        warnOnUnusedVerifications = false
      ),
      executableScriptName := name.value,
      dist in Universal := ((dist in Universal) dependsOn verify).value
    )
    .settings(inConfig(Integration)(Defaults.testSettings) : _*)
    .settings(inConfig(Benchmark)(Defaults.testSettings) : _*)
    .settings(inConfig(Evm)(Defaults.testSettings) : _*)
    .settings(inConfig(Ets)(Defaults.testSettings) : _*)
    .settings(inConfig(Snappy)(Defaults.testSettings) : _*)
    .settings(
      buildInfoKeys := Seq[BuildInfoKey](
        name,
        version,
        scalaVersion,
        sbtVersion,
        gitHeadCommit,
        gitCurrentBranch,
        gitDescribedVersion,
        gitUncommittedChanges,
        libraryDependencies in Compile
      ),
      useGitDescribe := true,
      buildInfoPackage := "io.iohk.ethereum.buildinfo",
      buildInfoObject := "MantisBuildInfo",
      buildInfoOptions += BuildInfoOption.ToMap
    )

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-Xlint:unsound-match",
  "-Ywarn-inaccessible",
  "-Ywarn-unused-import",
  "-encoding", "utf-8"
)

scalacOptions in (Compile, console) ~= (_.filterNot(Set(
  "-Ywarn-unused-import",
  "-Xfatal-warnings"
)))

parallelExecution in Test := false

testOptions in Test += Tests.Argument("-oD")

// protobuf compilation
//
// NOTE `sbt-protoc` and `sbt-buildinfo` do not work well together,
//      see https://github.com/sbt/sbt-buildinfo/issues/104
//      and https://github.com/thesamet/sbt-protoc/issues/6
//      That is why we generate protobuf code in another folder (`protobuf`).
PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value / "protobuf"
)

// have the protobuf API version file as a resource
unmanagedResourceDirectories in Compile += baseDirectory.value / "src" / "main" / "protobuf"

(test in Evm) := (test in Evm).dependsOn(solidityCompile).value
(sourceDirectory in Evm) := baseDirectory.value / "src" / "evmTest"

(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"
scalastyleSources in Test ++= {(unmanagedSourceDirectories in Integration).value}

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
