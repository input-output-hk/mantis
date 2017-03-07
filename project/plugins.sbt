logLevel := sbt.Level.Warn
addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "1.5.0")
addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "1.1.0")
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.0.6")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "latest.release")