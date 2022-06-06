name := "Orange Tracker"
version := "0.1"
scalaVersion := "3.1.2"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
     "org.scalatest" %% "scalatest" % "3.2.12" % "test")