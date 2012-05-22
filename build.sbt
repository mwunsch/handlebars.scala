name := "Handlebars"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.9" % "test",
  "org.slf4j" % "slf4j-api" % "1.6.4"
)

resolvers ++= Seq(
  "Sonatype.org Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype.org Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

scalacOptions += "-unchecked"
