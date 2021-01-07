logLevel := sbt.Level.Warn
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.7.5")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "3.0.0")
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.0-RC7")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.5.1")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.10.0"
