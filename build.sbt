enablePlugins(JavaAppPackaging, SolidityPlugin)

val commonSettings = Seq(
  name := "mantis",
  version := "0.3-cli-beta",
  scalaVersion := "2.12.1"
)

// Temp resolver for LevelDB fork
resolvers += "stepsoft" at "http://nexus.mcsherrylabs.com/repository/releases/"

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
    "commons-io" % "commons-io" % "2.5"
  )
}

val verifyDeps = Seq(
  "org.scala-lang" % "scala-library" sha1 "dd235d04037dc6f4b6090257872dd35359a563ce",
  "com.typesafe.akka" % "akka-actor" sha1 "ba05c9b5fb9ab1b9a7f2a14b94c71454d9ade820",
  "com.typesafe" % "config" sha1 "f533aa6ea13e443b50e639d070986c42d03efc35",
  "org.scala-lang.modules" % "scala-java8-compat" sha1 "1e6f1e745bf6d3c34d1e2ab150653306069aaf34",
  "com.typesafe.akka" % "akka-agent" sha1 "a902cb66c53d1376a877790265c7dabe672a0cb4",
  "org.scala-stm" % "scala-stm" sha1 "1ceeedf00f40697b77a459bdecd451b014960276",
  "com.typesafe.akka" % "akka-slf4j" sha1 "7091270169f7d27f5098be591a10cf1fba33c159",
  "com.typesafe.akka" % "akka-testkit" sha1 "475ffad312ccee76b79265a459ff02384ac0821c",
  "com.typesafe.akka" % "akka-http" sha1 "3141508ce76b029bfb437bb275f73c6f63554704",
  "com.typesafe.akka" % "akka-http-core" sha1 "3e312e31aaa8b012b582ebf12f751ada8402248f",
  "com.typesafe.akka" % "akka-parsing" sha1 "db7a5ce5708ff0f6feed6939bd15f98a5413c4e5",
  "com.typesafe.akka" % "akka-stream" sha1 "4633154311941f0db62a2418a31f49513a505e43",
  "org.reactivestreams" % "reactive-streams" sha1 "14b8c877d98005ba3941c9257cfe09f6ed0e0d74",
  "com.typesafe" % "ssl-config-core" sha1 "7497c001276c3fd76df8204f4611baaa24c5aea9",
  "org.scala-lang.modules" % "scala-parser-combinators" sha1 "3c1c5475ece77c41e18dd971f8f818c091e4961c",
  "ch.megard" % "akka-http-cors" sha1 "a86580cc415a343fa2e5d9aa8a6c6c7391f09372",
  "org.json4s" % "json4s-native" sha1 "5474e881ee64da870254517c2eb334fe466e5259",
  "org.json4s" % "json4s-core" sha1 "5390fb1ecb7c501e5d9a29ca3d00968e541e290a",
  "org.json4s" % "json4s-ast" sha1 "64dec965224fb6ac5ee742ca795716e7489b6402",
  "org.json4s" % "json4s-scalap" sha1 "fb05e4f3830064fb2ce9118b2f936c15f0879341",
  "com.thoughtworks.paranamer" % "paranamer" sha1 "619eba74c19ccf1da8ebec97a2d7f8ba05773dd6",
  "org.scala-lang.modules" % "scala-xml" sha1 "e22de3366a698a9f744106fb6dda4335838cf6a7",
  "de.heikoseeberger" % "akka-http-json4s" sha1 "575ba7e823282bf64ebcb4c04e33d9a6c24b5c31",
  "io.suzaku" % "boopickle" sha1 "85281da7833655a0fb7c91274c7a97cbe61b990c",
  "org.consensusresearch" % "scrypto" sha1 "20177c8d44689b7d3d398ab1f142daf42b8a978c",
  "com.chuusai" % "shapeless" sha1 "27e115ffed7917b456e54891de67173f4a68d5f1",
  "org.typelevel" % "macro-compat" sha1 "ed809d26ef4237d7c079ae6cf7ebd0dfa7986adf",
  "com.google.guava" % "guava" sha1 "6ce200f6b23222af3d8abb6b6459e6c44f4bb0e9",
  "org.whispersystems" % "curve25519-java" sha1 "09091eb56d696d0d0d70d00b84e6037dcd3d98b6",
  "com.madgag.spongycastle" % "core" sha1 "9622d6de1407dd3506254fb9b0292eb1206f6991",
  "org.iq80.leveldb" % "leveldb" sha1 "e9b071b63a7b40f7d01ae01e99259a2de72426f6",
  "org.iq80.leveldb" % "leveldb-api" sha1 "d71173b159a38acd8036d9694f1243afe6be9108",
  "org.scorexfoundation" % "iodb" sha1 "0d4b86fe17008bfc5ec0fe4317d6d9c39a81dc85",
  "net.jpountz.lz4" % "lz4" sha1 "c708bb2590c0652a642236ef45d9f99ff842a2ce",
  "org.slf4j" % "slf4j-api" sha1 "432be7c915d6389efd927e32937a30f7d5556f3e",
  "ch.qos.logback" % "logback-classic" sha1 "978cd9fbb43b7abed6379d7b02de052d216e30fc",
  "ch.qos.logback" % "logback-core" sha1 "e05d0cb67220937c32d7b4e5a47f967605376f63",
  "org.jline" % "jline" sha1 "dfb4e9e15e981634155ce063fa697b2b8964d507",
  "io.circe" % "circe-core" sha1 "a2f4e27c41844fac377a7c26f50fe4b6f667c855",
  "io.circe" % "circe-numbers" sha1 "8e97dd54d3bca34c730f19b28bce23f4a052302a",
  "org.typelevel" % "cats-core" sha1 "267cebe07afbb365b08a6e18be4b137508f16bee",
  "org.typelevel" % "cats-macros" sha1 "4733f8227b3a64bbd3be749c682d456b66e4dd6e",
  "com.github.mpilquist" % "simulacrum" sha1 "043c9efadda1dc59a5d1a73ce77b145074c7fd35",
  "org.typelevel" % "machinist" sha1 "13f7388cf36bcecf51bde7b87a216d5aa101ae2a",
  "org.scala-lang" % "scala-reflect" sha1 "f6ae9e1c0204a3e92893d9a2188b276278f2074e",
  "org.typelevel" % "cats-kernel" sha1 "24eae5d3c4b0c532b107efe36519324c0c8f03c0",
  "io.circe" % "circe-generic" sha1 "38a949ac611a8d48c80fd8c0736d55688d08e69e",
  "io.circe" % "circe-parser" sha1 "009d8fce67711164d8f17f0b05fce4d0daa44736",
  "io.circe" % "circe-jawn" sha1 "23890a0fa474c84ca5b39312425c0b9879467cd5",
  "org.spire-math" % "jawn-parser" sha1 "452b1bdf2982219e1b7c9d27ec316241144b0910",
  "io.circe" % "circe-generic-extras" sha1 "f9b74914b6fd7193b221bb0eed8d1a82f7fa2aef",
  "commons-io" % "commons-io" sha1 "2852e6e05fbb95076fc091f6d1780f1f8fe35e0f"
)

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
      verifyDependencies in verify ++= verifyDeps,
      verifyOptions in verify := VerifyOptions(
        includeBin = true,
        includeScala = true,
        includeDependency = true,
        excludedJars = Nil,
        warnOnUnverifiedFiles = false,
        warnOnUnusedVerifications = false
      ),
      dist in Universal := ((dist in Universal) dependsOn verify).value
    )
    .settings(inConfig(Integration)(Defaults.testSettings) : _*)
    .settings(inConfig(Benchmark)(Defaults.testSettings) : _*)
    .settings(inConfig(Evm)(Defaults.testSettings) : _*)
    .settings(inConfig(Ets)(Defaults.testSettings) : _*)
    .settings(inConfig(Snappy)(Defaults.testSettings) : _*)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-Xlint:unsound-match",
  "-Ywarn-inaccessible",
  "-Ywarn-unused-import"
)

scalacOptions in (Compile, console) ~= (_.filterNot(Set(
  "-Ywarn-unused-import",
  "-Xfatal-warnings"
)))

parallelExecution in Test := false

testOptions in Test += Tests.Argument("-oD")

(test in Evm) := (test in Evm).dependsOn(solidityCompile).value
(sourceDirectory in Evm) := baseDirectory.value / "src" / "evmTest"

(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"
scalastyleSources in Test ++= {(unmanagedSourceDirectories in Integration).value}

mainClass in Compile := Some("io.iohk.ethereum.App")
