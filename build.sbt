lazy val root = project
  .in(file("."))
  .settings(
    name := "simple-http-library",
    description := "Simple http library using Scala 3",
    version := "0.1.0",
    scalaVersion := "3.0.0",
    libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.9" % "test", "com.typesafe.akka" % "akka-actor_2.13" % "2.6.15")
  )
