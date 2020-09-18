import sbt._

object Dependencies {

  val akka: Seq[ModuleID] = {
    val akkaVersion = "2.6.9"

    Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
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

  val json4s = Seq("org.json4s" %% "json4s-native" % "3.5.4")

  val circe: Seq[ModuleID] = {
    val circeVersion = "0.12.1" //0.9.3

    Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-generic-extras" % circeVersion
    )
  }

  val boopickle = Seq("io.suzaku" %% "boopickle" % "1.3.0")

  val rocksDb = Seq(
    "org.rocksdb" % "rocksdbjni" % "6.11.4"
  )

  val enumeratum: Seq[ModuleID] = Seq(
    "com.beachape" %% "enumeratum" % "1.5.13",
    "com.beachape" %% "enumeratum-cats" % "1.5.15",
    "com.beachape" %% "enumeratum-scalacheck" % "1.5.16" % Test
  )

  val testing: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.0.8" % "it,test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.1" % "it,test"
  )

  val cats: Seq[ModuleID] = {
    val catsVersion = "2.0.0"
    Seq(
      "org.typelevel" %% "mouse" % "0.23",
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-effect" % catsVersion
    )
  }

  val monix = Seq(
    "io.monix" %% "monix" % "3.1.0"
  )

  val logging = Seq(
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
    "net.logstash.logback" % "logstash-logback-encoder" % "6.4",
    "org.codehaus.janino" % "janino" % "3.0.6"
  )

  val twitterUtilCollection = Seq("com.twitter" %% "util-collection" % "18.5.0")

  val crypto = Seq("org.bouncycastle" % "bcprov-jdk15on" % "1.59")

  val scopt = Seq("com.github.scopt" % "scopt_2.12" % "3.7.0")

  val metrics = Seq(
    // Metrics (https://github.com/DataDog/java-dogstatsd-client)
    "com.datadoghq" % "java-dogstatsd-client" % "2.5",
    "org.xerial.snappy" % "snappy-java" % "1.1.7.2",
    "org.web3j" % "core" % "3.4.0" % "test"
  )

  val apacheCommons = Seq(
    "commons-io" % "commons-io" % "2.6"
  )

  val dependencies = Seq(
    "org.jline" % "jline" % "3.1.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
    "org.scala-sbt.ipcsocket" % "ipcsocket" % "1.0.0",
    "com.google.guava" % "guava" % "28.0-jre"
  )

}
