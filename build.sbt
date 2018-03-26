enablePlugins(JDKPackagerPlugin, JavaAppPackaging, SolidityPlugin)

val commonSettings = Seq(
  name := "mantis",
  version := "1.0-daedalus-rc1",
  scalaVersion := "2.12.1",
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-l", "MinerSpec") // miner tests disabled by default
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
    "org.bouncycastle" % "bcprov-jdk15on" % "1.59"
  )
}

val verifyDeps = Seq(
  "org.scala-lang" % "scala-library" sha256 "9dab78f3f205a038f48183b2391f8a593235f794d8129a479e06af3e6bc50ef8",
  "com.typesafe.akka" % "akka-actor" sha256 "85d8d9d856eb8aa3619f6b2f7ad4e6c31b4bcc22bc20cb6f8be6a0ce5a4c74fa",
  "com.typesafe" % "config" sha256 "d3e9dca258786c51fcbcc47d34d3b44158476af55c47d22dd8c2e38e41a2c89a",
  "org.scala-lang.modules" % "scala-java8-compat" sha256 "d9d5dfd1bc49a8158e6e0a90b2ed08fa602984d815c00af16cec53557e83ef8e",
  "com.typesafe.akka" % "akka-agent" sha256 "1e2f3f150114213d90289b6b8986e547f984c4cd8100b7738c9e7119e105453d",
  "org.scala-stm" % "scala-stm" sha256 "307d61bbbc4e6ed33881646f23140ac73d71a508452abdbb8da689e64a1e4d93",
  "com.typesafe.akka" % "akka-slf4j" sha256 "1226a10703d60a0926d0113255fcd0cc92728ee67d960ff66c0f4a76cde330f6",
  "com.typesafe.akka" % "akka-testkit" sha256 "7bf49fc5602278e694d2b125325ae303085ac9442e56a4b7decb71f627bfff84",
  "com.typesafe.akka" % "akka-http" sha256 "1b03021aa2097f9ebf40f5e600eaf56518321bc7f671ab11037767928983e460",
  "com.typesafe.akka" % "akka-http-core" sha256 "5cabc6e8152f7210891dd497cd2ac8b05331b51d342aa8b0ee8a278dca523475",
  "com.typesafe.akka" % "akka-parsing" sha256 "de2e096b51d88b3462aeff1a086e77cfeb848572a70e4339da38c088c5a3b9a5",
  "com.typesafe.akka" % "akka-stream" sha256 "0cded6f4225ca70bc1b94f47014b61c8b0c0feaad9c2178c247f854ba0356735",
  "org.reactivestreams" % "reactive-streams" sha256 "ef867702a614b96eb6c64fb65a8f5e14bdfcabbc1ae056f78a1643f7b79ca0eb",
  "com.typesafe" % "ssl-config-core" sha256 "c169a9846e1259e33348d406dd64ca1556a35a404de2a96871effcea9f0998ce",
  "org.scala-lang.modules" % "scala-parser-combinators" sha256 "f1f2f43cfd8042eb8a5d3021dc7ac3fff08ed6565311b6c145f8efe882a58a75",
  "ch.megard" % "akka-http-cors" sha256 "15132b781f9979d33d67b5cdefd63296101e79b7d51766591b04e5d1d44f6183",
  "org.json4s" % "json4s-native" sha256 "9cdd85a0fa32932b1d5b50f0b1360b6564c636242e72c68a4f91504df0edf568",
  "org.json4s" % "json4s-core" sha256 "23b0c72b8a8e960c8c8cfa3cb09fc22ef078c397a9fa5f677dbded7deeeb5108",
  "org.json4s" % "json4s-ast" sha256 "9eebcb541468e05a5bf9e2a7e1c8f59fe0a8cdef2c77e66ebeda8e323591fe32",
  "org.json4s" % "json4s-scalap" sha256 "14077cae3da09ccc58f55d2c9bac8345ed58771e1d789fff4f1912774e2835d8",
  "com.thoughtworks.paranamer" % "paranamer" sha256 "688cb118a6021d819138e855208c956031688be4b47a24bb615becc63acedf07",
  "org.scala-lang.modules" % "scala-xml" sha256 "7cc3b6ceb56e879cb977e8e043f4bfe2e062f78795efd7efa09f85003cb3230a",
  "de.heikoseeberger" % "akka-http-json4s" sha256 "71a6c7bea27441217bb0a198256c5a221cd725f30cd5643d104caeb5e29a0e03",
  "io.suzaku" % "boopickle" sha256 "b0eb1443f3d807e0ca2c44a3acad5f93ac4426ea03d7fbc43051a40180cbefc0",
  "org.iq80.leveldb" % "leveldb" sha256 "61b61247f6be0a29deeb00793b61a7982b0d59bb3f2e01a208f1908a97f4ef31",
  "org.iq80.leveldb" % "leveldb-api" sha256 "0b1f0811f2034021fdc7af655358570f0bdfee16cbfda1e72be0b9a96f3414db",
  "org.scorexfoundation" % "iodb" sha256 "0004bd8fee6ece8f7896d64cb08c7857baffe04fa585802cb94ebd8f487d3a05",
  "com.google.guava" % "guava" sha256 "58d4cc2e05ebb012bbac568b032f75623be1cb6fb096f3c60c72a86f7f057de4",
  "net.jpountz.lz4" % "lz4" sha256 "b877a4d4a3a0140486d3d0f83d9058e7c0ff6ca80b00d2f7b77145935b385b56",
  "org.slf4j" % "slf4j-api" sha256 "364a2b6afd201cf7d69127d127aebc757356c425f3932e099bcba34d882621b9",
  "ch.qos.logback" % "logback-classic" sha256 "1a9510fea0e644968adca6d35300483772cb7b1ddcdc84c5fdf25f68d450e355",
  "ch.qos.logback" % "logback-core" sha256 "19346df199c443f56b4880d386016295d628293643152f5f4ac6287a341ada74",
  "org.jline" % "jline" sha256 "eca7a261eff7c06005896cba16ac7835dfc2204ce5bea63935d1e50b1968b4da",
  "io.circe" % "circe-core" sha256 "834043cdc163d34650d3dd09d64389650728d552d583d7a341be21432b59c785",
  "io.circe" % "circe-numbers" sha256 "43894f77dcc5e316deac4f2dc39acd7311be23203bfc06f8b43a6f79014b2fcc",
  "org.typelevel" % "cats-core" sha256 "3ca705cba9dc0632e60477d80779006f8c636c0e2e229dda3410a0c314c1ea1d",
  "org.typelevel" % "cats-macros" sha256 "0bc0eeed47f094a0daf754bd6022c4b8c3093bafed1142ca7231e2b58d8908b1",
  "com.github.mpilquist" % "simulacrum" sha256 "ab973ec7cf3ac1cbe11a3d866aae7b567bb4b2a7f038b9ee21ba3ce6b177613e",
  "org.typelevel" % "macro-compat" sha256 "8b1514ec99ac9c7eded284367b6c9f8f17a097198a44e6f24488706d66bbd2b8",
  "org.typelevel" % "machinist" sha256 "fee6035ab2db522083775b2d97f192fc76bb7d4eed5151081e6933bf3da800e6",
  "org.scala-lang" % "scala-reflect" sha256 "d8a2b9d6d78c7457a40e394dc0c4fa6d6244acf0d156bbbcb311a9d497b85eec",
  "org.typelevel" % "cats-kernel" sha256 "b544a2aed029bbf71e1d1dea9346a21a040f9ddfe1828578e73b01f938177006",
  "io.circe" % "circe-generic" sha256 "c2dd19c7d23b133bef887a6adedefbfa2dcdd93a61ba97addef0bc8e10c17fdb",
  "com.chuusai" % "shapeless" sha256 "75926d9dd4688710ca16d852b58746dcfc013a2a1a58d1e817a27f95b2d42303",
  "io.circe" % "circe-parser" sha256 "c8d92d46373201a6b32ee72b9b5370a982456a553e4645a3c831f0d179e8d492",
  "io.circe" % "circe-jawn" sha256 "ed1392a9179d2a6093e33ac05c040bdebda004b897728491e2e1f345de750347",
  "org.spire-math" % "jawn-parser" sha256 "c617fdde8c5b7646b1bedc4f6f565e85aa83b157ea93977fcdc4056b823aadb2",
  "io.circe" % "circe-generic-extras" sha256 "f908059ccc579a993db60fe235d40fb7dfa7b9e8b85613fabac714eb167bd953",
  "commons-io" % "commons-io" sha256 "a10418348d234968600ccb1d988efcbbd08716e1d96936ccc1880e7d22513474",
  "org.bouncycastle" % "bcprov-jdk15on" sha256 "1c31e44e331d25e46d293b3e8ee2d07028a67db011e74cb2443285aed1d59c85"
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
  "-Ywarn-unused-import",
  "-encoding", "utf-8"
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

// Requires the 'ant-javafx.jar' that comes with Oracle JDK
// Enables creating an executable with the configuration files, has to be run on the OS corresponding to the desired version
jdkPackagerType := "image"

val sep = java.io.File.separator
jdkPackagerJVMArgs := Seq(
  "-Dconfig.file=." + sep + "conf" + sep + "mantis.conf",
  "-Dlogback.configurationFile=." + sep + "conf" + sep + "logback.xml",
  "-Xss10M"
)
