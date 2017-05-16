name := """pure-fun-cqrs-es"""

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")




