def scala211Dependencies(scalaVersion:String) = {
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq(
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.7")
    case _ =>
      Seq()
  }
}

val updateVersion = taskKey[Unit]("Updates version in README")
libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.6.4",
  "org.slf4j" % "slf4j-simple" % "1.6.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
) ++ scala211Dependencies(scalaVersion.value)

val commonSettings = Seq(
  organization := "com.gilt",

  // scalaVersion := "2.11.7",
  scalaVersion := "2.12.4",

  crossScalaVersions := Seq("2.12.4", "2.11.7", "2.10.5"),

  libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-api" % "1.6.4",
    "org.slf4j" % "slf4j-simple" % "1.6.4" % "test",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  ) ++ scala211Dependencies(scalaVersion.value),

  updateVersion := {
    val log = streams.value.log
    val readme = (baseDirectory.value / "README.md")
    val content = IO.read(readme)
    val regex = """("handlebars-scala[a-z-]*" % ")[0-9\.]+(")""".r
    val replaced = regex.replaceAllIn(content, { m => s"${m.group(1)}${version.value}${m.group(2)}"})

    IO.write(readme, replaced)
    log.info(s"Updated ${readme}")
  },

  resolvers ++= Seq(
    "Sonatype.org Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype.org Releases" at "http://oss.sonatype.org/service/local/staging/deploy/maven2"
  ),

  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked"
  ),

  publishMavenStyle := true,

  /* Disabling Sontatype publishing in my fork
  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "/service/local/staging/deploy/maven2")
  },
  */

  // For publishing / testing locally
  publishTo := Some(Resolver.file("m2",  new File(Path.userHome.absolutePath+"/.m2/repository"))),

  publishArtifact in Test := false,

  pomIncludeRepository := { _ => false },

  licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),

  homepage := Some(url("https://github.com/mwunsch/handlebars.scala")),

  pomExtra := (
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
        <developer>
          <id>chicks</id>
          <name>Chris Hicks</name>
          <url>http://tech.gilt.com/</url>
            <organization>Gilt</organization>
          <organizationUrl>http://www.gilt.com</organizationUrl>
        </developer>
        <developer>
          <id>timcharper</id>
          <name>Tim Harper</name>
          <url>http://timcharper.com/</url>
          <organization>Foundational Software</organization>
          <organizationUrl>http://www.foundationalsoftware.com</organizationUrl>
        </developer>
      </developers>
  )
)

lazy val core = (project in file("./")).
  settings(commonSettings: _*).
  settings(
    name := "handlebars-scala")

lazy val `play-json` = (project in file("./addons/play-json/")).
  settings(commonSettings: _*).
  settings(
    name := "handlebars-scala-play-json",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.9").
  dependsOn(core)

lazy val `all` = (project in file("./addons/all")).
  settings(commonSettings: _*).
  settings(
    updateVersion := {},
    publish := {},
    test := {}
  ).
  dependsOn(core, `play-json`).
  aggregate(core, `play-json`)
