name := "Orange Tracker"
version := "1.00.000-SNAPSHOT"
scalaVersion := "3.3.1"
scalacOptions ++= Seq("-deprecation", "-feature")

javaOptions ++= Seq(
  "--add-opens=java.base/java.lang=ALL-UNNAMED",
  "-XX:MaxMetaspaceSize=6024m", // Set the MaxMetaspaceSize to 256 megabytes
  // Other JVM options can be added here as well
)

libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" % "2.0.0",
     "org.scalatest" %% "scalatest" % "3.2.10" % "test")
