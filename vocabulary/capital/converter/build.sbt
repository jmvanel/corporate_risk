organization := "deductions"

name := "md2rdf"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.8"

javacOptions ++= Seq("-source","1.7", "-target","1.7")

libraryDependencies += "com.tristanhunt" %% "knockoff" % "0.8.3"

resolvers += Resolver.file("Local repo", file(System.getProperty("user.home") + "/.ivy2/local"))(Resolver.ivyStylePatterns)

libraryDependencies += "junit" % "junit" % "4.8.1" % Test

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % Test

// scalariformSettings

