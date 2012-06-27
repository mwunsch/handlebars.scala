name := "Handlebars"

organization := "com.gilt"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.9" % "test",
  "org.slf4j" % "slf4j-api" % "1.6.4"
)

resolvers ++= Seq(
  "Sonatype.org Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype.org Releases" at "http://oss.sonatype.org/service/local/staging/deploy/maven2"
)

scalacOptions += "-unchecked"

crossPaths := false

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("Sonatype.org Snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("Sonatype.org Releases"  at nexus + "/service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/gilt/handlebars.scala</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:gilt/handlebars.scala.git</url>
    <connection>scm:git:git@github.com:gilt/handlebars.scala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>mwunsch</id>
      <name>Mark Wunsch</name>
      <url>http://markwunsch.com/</url>
    </developer>
  </developers>)
