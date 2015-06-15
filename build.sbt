lazy val scalatraVersion = "2.3.1"

lazy val `ios-algorithm` = (project in file(".")).settings(
  name := "ios-algorithm",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.6",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  libraryDependencies ++= Seq(
    "org.scalatra"      %% "scalatra"           % scalatraVersion,
    "org.scalatra"      %% "scalatra-scalate"   % scalatraVersion,
    "org.scalatra"      %% "scalatra-specs2"    % scalatraVersion    % "test",
    "junit" % "junit" % "4.12" % "test",
    "org.scalatest"  %% "scalatest"  % "2.2.4"  % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.3" % "test",
    "org.scalatra"      %% "scalatra-json"      % scalatraVersion,
    "org.json4s"        % "json4s-jackson_2.11" % "3.2.11",
    "ch.qos.logback"    %  "logback-classic"    % "1.1.3"            % "runtime",
    "org.eclipse.jetty" %  "jetty-webapp"       % "9.2.10.v20150310" % "container",
    "javax.servlet"     %  "javax.servlet-api"  % "3.1.0"            % "provided",
    "org.scalaj"        %% "scalaj-http"        % "1.1.4",
    "ch.qos.logback"    % "logback-classic"     % "1.1.3"            % "runtime"
  )
).settings(jetty(): _*)