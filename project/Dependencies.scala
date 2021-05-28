import sbt._

object Dependencies {

  private val akkaVersion = "2.6.9"

  val akkaUtil: Seq[ModuleID] = {
    Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion
    )
  }

  val akka: Seq[ModuleID] = {
    Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
      "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion,
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,
      "com.miguno.akka" %% "akka-mock-scheduler" % "0.5.5" % "it,test"
    )
  }

  val akkaHttp: Seq[ModuleID] = {
    val akkaHttpVersion = "10.2.0"

    Seq(
      "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
      "ch.megard" %% "akka-http-cors" % "1.1.0",
      "de.heikoseeberger" %% "akka-http-json4s" % "1.34.0",
      "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "it,test"
    )
  }

  val json4s = Seq("org.json4s" %% "json4s-native" % "3.6.9")

  val circe: Seq[ModuleID] = {
    val circeVersion = "0.13.0"

    Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-generic-extras" % circeVersion
    )
  }

  val boopickle = Seq("io.suzaku" %% "boopickle" % "1.3.3")

  val rocksDb = Seq(
    // use "5.18.3" for older macOS
    "org.rocksdb" % "rocksdbjni" % "6.15.2"
  )

  val enumeratum: Seq[ModuleID] = Seq(
    "com.beachape" %% "enumeratum" % "1.6.1",
    "com.beachape" %% "enumeratum-cats" % "1.6.1",
    "com.beachape" %% "enumeratum-scalacheck" % "1.6.1" % Test
  )

  val testing: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.2.2" % "it,test",
    "org.scalamock" %% "scalamock" % "5.0.0" % "it,test",
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.3.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.15.1" % "it,test",
    "com.softwaremill.diffx" %% "diffx-core" % "0.3.30" % "test",
    "com.softwaremill.diffx" %% "diffx-scalatest" % "0.3.30" % "test"
  )

  val cats: Seq[ModuleID] = {
    val catsVersion = "2.2.0"
    Seq(
      "org.typelevel" %% "mouse" % "0.25",
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-effect" % catsVersion
    )
  }

  val monix = Seq(
    "io.monix" %% "monix" % "3.2.2"
  )

  val network: Seq[ModuleID] = {
    val scalanetVersion = "0.6.0"
    Seq(
      "io.iohk" %% "scalanet" % scalanetVersion,
      "io.iohk" %% "scalanet-discovery" % scalanetVersion
    )
  }

  val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
    "net.logstash.logback" % "logstash-logback-encoder" % "6.4",
    "org.codehaus.janino" % "janino" % "3.1.2"
  )

  val crypto = Seq("org.bouncycastle" % "bcprov-jdk15on" % "1.66")

  val scopt = Seq("com.github.scopt" %% "scopt" % "4.0.0")

  val cli = Seq("com.monovore" %% "decline" % "1.3.0")

  val apacheCommons = Seq(
    "commons-io" % "commons-io" % "2.8.0"
  )

  val jline = "org.jline" % "jline" % "3.16.0"

  val jna = "net.java.dev.jna" % "jna" % "5.6.0"

  val dependencies = Seq(
    jline,
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "org.scala-sbt.ipcsocket" % "ipcsocket" % "1.1.0",
    "org.xerial.snappy" % "snappy-java" % "1.1.7.7",
    "org.web3j" % "core" % "4.5.11" % Test,
    "io.vavr" % "vavr" % "1.0.0-alpha-3",
    "org.jupnp" % "org.jupnp" % "2.5.2",
    "org.jupnp" % "org.jupnp.support" % "2.5.2",
    "org.jupnp" % "org.jupnp.tool" % "2.5.2",
    "javax.servlet" % "javax.servlet-api" % "4.0.1"
  )

  val guava: Seq[ModuleID] = {
    val version = "30.1-jre"
    Seq(
      "com.google.guava" % "guava" % version,
      "com.google.guava" % "guava-testlib" % version % "test"
    )
  }

  val prometheus: Seq[ModuleID] = {
    val provider = "io.prometheus"
    val version = "0.9.0"
    Seq(
      provider % "simpleclient" % version,
      provider % "simpleclient_logback" % version,
      provider % "simpleclient_hotspot" % version,
      provider % "simpleclient_httpserver" % version
    )
  }

  val micrometer: Seq[ModuleID] = {
    val provider = "io.micrometer"
    val version = "1.5.5"
    Seq(
      // Required to compile metrics library https://github.com/micrometer-metrics/micrometer/issues/1133#issuecomment-452434205
      "com.google.code.findbugs" % "jsr305" % "3.0.2" % Optional,
      provider % "micrometer-core" % version,
      provider % "micrometer-registry-jmx" % version,
      provider % "micrometer-registry-prometheus" % version
    )
  }

  val kamon: Seq[ModuleID] = {
    val provider = "io.kamon"
    val version = "2.1.9"
    Seq(
      provider %% "kamon-prometheus" % version,
      provider %% "kamon-akka" % version
    )
  }

  val shapeless: Seq[ModuleID] = Seq(
    "com.chuusai" %% "shapeless" % "2.3.3"
  )

  val scaffeine: Seq[ModuleID] = Seq(
    "com.github.blemale" %% "scaffeine" % "4.0.2" % "compile"
  )

}
