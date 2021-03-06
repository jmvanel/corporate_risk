addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

// collé de Banana, pas forcement utile:
resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
  url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
  Resolver.ivyStylePatterns)

// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
// addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.6")
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.10")
// addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.6")

// addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "3.0.0")

