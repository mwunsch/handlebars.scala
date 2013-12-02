name := "Handlebars"

organization := "com.gilt"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.9.2"

crossPaths := false

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.slf4j" % "slf4j-simple" % "1.7.5"
)

resolvers ++= Seq(
  "Sonatype.org Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype.org Releases" at "http://oss.sonatype.org/service/local/staging/deploy/maven2"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked"
)

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

testOptions in Test += Tests.Argument("-oD")

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/mwunsch/handlebars.scala</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0</url>
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
      <organization>Gilt</organization>
      <organizationUrl>http://www.gilt.com</organizationUrl>
    </developer>
  </developers>)
