organization := "bizinnov"
name := "corporate_risk"
version := "1.0-SNAPSHOT"

scalaVersion := "2.11.8"
javacOptions ++= Seq("-source","1.7", "-target","1.7")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % Test

libraryDependencies += "deductions" %% "semantic_forms" % "1.0-SNAPSHOT"
libraryDependencies += "com.typesafe.play.plugins" %% "play-plugins-mailer" % "2.3.1"
libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "0.5.0" // -SNAPSHOT" // latest.integration"

resolvers += Resolver.file("Local repo", file(System.getProperty("user.home") + "/.ivy2/local"))(Resolver.ivyStylePatterns)
// cf http://stackoverflow.com/questions/16400877/local-dependencies-resolved-by-sbt-but-not-by-play-framework

lazy val playApp = (project in file(".")).enablePlugins(PlayScala)

scalariformSettings
