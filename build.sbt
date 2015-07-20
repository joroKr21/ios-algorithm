organization := "com.groupby"

name := "ios-algorithm"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.7"

mainClass in (Compile, run) := Some("assigner.service.SprayService")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

val versions = new {
  val akka       = "2.3.9"
  val junit      = "4.12"
  val scalaCheck = "1.12.2"
  val scalaTest  = "2.2.4"
  val spray      = "1.3.3"
}

libraryDependencies ++= Seq( // Spray
  "io.spray" %% "spray-can"     % versions.spray,
  "io.spray" %% "spray-json"    % "1.3.2",
  "io.spray" %% "spray-routing" % versions.spray,
  "io.spray" %% "spray-testkit" % versions.spray  % "test")

libraryDependencies ++= Seq( // Akka
  "com.typesafe.akka" %% "akka-actor"    % versions.akka,
  "com.typesafe.akka" %% "akka-testkit"  % versions.akka % "test")

libraryDependencies ++= Seq( // Testing
  "junit"          %  "junit"      % versions.junit      % "test",
  "org.scalacheck" %% "scalacheck" % versions.scalaCheck % "test",
  "org.scalatest"  %% "scalatest"  % versions.scalaTest  % "test")

libraryDependencies += "net.debasishg" %% "redisclient" % "3.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" % "runtime"

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.11"

Revolver.settings.settings
