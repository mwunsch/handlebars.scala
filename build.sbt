name := "Handlebars"

organization := "com.gilt"

version := "0.0.18"

scalaVersion := "2.9.1"

crossPaths := false

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.9" % "test",
  "org.slf4j" % "slf4j-api" % "1.6.4",
  "org.slf4j" % "slf4j-simple" % "1.6.4"
)

libraryDependencies += "com.google.guava" % "guava" % "12.0"

resolvers ++= Seq(
  "Sonatype.org Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype.org Releases" at "http://oss.sonatype.org/service/local/staging/deploy/maven2"
)

scalacOptions += "-unchecked"

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "/service/local/staging/deploy/maven2")
}

// For publishing / testing locally
// publishTo := Some(Resolver.file("m2",  new File(Path.userHome.absolutePath+"/.m2/repository")))

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/mwunsch/handlebars.scala</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:mwunsch/handlebars.scala.git</url>
    <connection>scm:git:git@github.com:mwunsch/handlebars.scala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>mwunsch</id>
      <name>Mark Wunsch</name>
      <url>http://markwunsch.com/</url>
    </developer>
  </developers>)
