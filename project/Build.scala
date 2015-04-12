import sbt._
import Keys._
import sbtbuildinfo.Plugin._

object ScalikeJDBCProjects extends Build {

  lazy val _version = "2.2.6-SNAPSHOT"

  lazy val _organization = "org.scalikejdbc"

  // published dependency version
  lazy val _slf4jApiVersion = "1.7.10"
  lazy val _typesafeConfigVersion = "1.2.1"

  // internal only
  lazy val _logbackVersion = "1.1.2"
  lazy val _h2Version = "1.4.186"
  lazy val _mysqlVersion = "5.1.34"
  lazy val _postgresqlVersion = "9.4-1201-jdbc41"
  lazy val _hibernateVersion = "4.3.8.Final"
  lazy val _scalatestVersion = "2.2.4"
  lazy val _specs2Version = "2.4.16"

  private def gitHash: String = try {
    sys.process.Process("git rev-parse HEAD").lines_!.head
  } catch {
    case e: Exception =>
      println(e)
      "master"
  }

  lazy val baseSettings = Seq(
    organization := _organization,
    version := _version,
    publishTo <<= version { (v: String) => _publishTo(v) },
    publishMavenStyle := true,
    resolvers ++= _resolvers,
    transitiveClassifiers in Global := Seq(Artifact.SourceClassifier),
    incOptions := incOptions.value.withNameHashing(true),
    //scalaVersion := "2.11.6",
    scalacOptions ++= _scalacOptions,
    scalacOptions in (Compile, doc) ++= Seq(
      "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
      "-doc-source-url", s"https://github.com/scalikejdbc/scalikejdbc/tree/${gitHash}€{FILE_PATH}.scala"
    ),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false },
    logBuffered in Test := false,
    parallelExecution in Test := false,
    pomExtra := _pomExtra
  )

  val root211Id = "root211"
  val mapperGeneratorId = "mapper-generator"

  lazy val root211 = Project(
    root211Id,
    file("root211")
  ).settings(
    baseSettings: _*
  ).settings(
    commands += Command.command("testSequential"){
      projects.map(_.id).filterNot(Set(root211Id, mapperGeneratorId)).map(_ + "/test").sorted ::: _
    }
  ).copy(
    aggregate = projects.filterNot(p => Set(root211Id, mapperGeneratorId).contains(p.id)).map(p => p: ProjectReference)
  )

  // scalikejdbc-mapper-generator-core
  // core library for mapper-generator
  lazy val scalikejdbcMapperGeneratorCore = Project(
    id = "mapper-generator-core",
    base = file("scalikejdbc-mapper-generator-core"),
    settings = baseSettings ++ Seq(
      name := "scalikejdbc-mapper-generator-core",
      libraryDependencies ++= {
        Seq(
          "org.slf4j"     %  "slf4j-api" % _slf4jApiVersion   % "compile",
          "org.scalikejdbc" %% "scalikejdbc-test" % "2.2.5" % "compile"
        ) ++
          scalaTestDependenciesInTestScope ++
          specs2DependenciesInTestScope ++
          jdbcDriverDependenciesInTestScope
      }
    )
  )

  // mapper-generator sbt plugin
  lazy val scalikejdbcMapperGenerator = Project(
    id = mapperGeneratorId,
    base = file("scalikejdbc-mapper-generator"),
    settings = baseSettings ++ ScriptedPlugin.scriptedSettings ++ Seq(
      sbtPlugin := true,
      ScriptedPlugin.scriptedBufferLog := false,
      ScriptedPlugin.scriptedLaunchOpts ++= sys.process.javaVmArguments.filter(
        a => Seq("-XX","-Xss").exists(a.startsWith)
      ) ++ Seq("-Xmx3G"),
      ScriptedPlugin.scriptedLaunchOpts ++= Seq(
        "-Dplugin.version=" + version.value,
        "-Dslf4j.version=" + _slf4jApiVersion,
        "-Dmysql.version=" + _mysqlVersion,
        "-Dpostgresql.version=" + _postgresqlVersion,
        "-Dh2.version=1.4.181",
        "-Dspecs2.version=" + _specs2Version,
        "-Dscalatest.version=" + _scalatestVersion
      ),
      name := "scalikejdbc-mapper-generator",
      libraryDependencies ++= {
        Seq(
          "org.slf4j"     %  "slf4j-simple" % _slf4jApiVersion  % "compile",
          "org.scalikejdbc" %% "scalikejdbc-core" % "2.2.5" % "compile",
          "org.scalikejdbc" %% "scalikejdbc-test" % "2.2.5" % "compile"
        ) ++
          scalaTestDependenciesInTestScope ++
          specs2DependenciesInTestScope ++
          jdbcDriverDependenciesInTestScope
      }
    )
  ) dependsOn(scalikejdbcMapperGeneratorCore)

  def macroDependenciesInCompileScope(scalaVersion: String) = {
    if (scalaVersion.startsWith("2.10")) Seq(
      "org.scalamacros" %% "quasiquotes" % "2.0.1" % "compile",
      compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
    ) else Seq()
  }

  def _publishTo(v: String) = {
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
  val _resolvers = Seq(
    "typesafe repo" at "http://repo.typesafe.com/typesafe/releases",
    "sonatype releases" at "https://oss.sonatype.org/content/repositories/releases",
    "sonatype snaphots" at "https://oss.sonatype.org/content/repositories/snapshots"
  )
  lazy val scalaTestDependenciesInTestScope =
    Seq("org.scalatest" %% "scalatest" % _scalatestVersion % "test")

  lazy val specs2DependenciesInTestScope =
    Seq("org.specs2" %% "specs2-core" % _specs2Version % "test")

  val jdbcDriverDependenciesInTestScope = Seq(
    "com.h2database"    % "h2"                   % _h2Version         % "test",
    "org.apache.derby"  % "derby"                % "10.11.1.1"        % "test",
    "org.xerial"        % "sqlite-jdbc"          % "3.8.7"            % "test",
    "org.hsqldb"        % "hsqldb"               % "2.3.2"            % "test",
    "mysql"             % "mysql-connector-java" % _mysqlVersion      % "test",
    "org.postgresql"    % "postgresql"           % _postgresqlVersion % "test"
  )
  //val _scalacOptions = Seq("-deprecation", "-unchecked", "-Ymacro-debug-lite", "-Xlog-free-terms", "Yshow-trees", "-feature")
  val _scalacOptions = Seq("-deprecation", "-unchecked", "-feature")
  val _pomExtra = <url>http://scalikejdbc.org/</url>
      <licenses>
        <license>
          <name>Apache License, Version 2.0</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:scalikejdbc/scalikejdbc-mapper-generator.git</url>
        <connection>scm:git:git@github.com:scalikejdbc/scalikejdbc-mapper-generator.git</connection>
      </scm>
      <developers>
        <developer>
          <id>seratch</id>
          <name>Kazuhiro Sera</name>
          <url>http://git.io/sera</url>
        </developer>
        <developer>
          <id>tototoshi</id>
          <name>Toshiyuki Takahashi</name>
          <url>https://github.com/tototoshi</url>
        </developer>
        <developer>
          <id>xuwei-k</id>
          <name>Kenji Yoshida</name>
          <url>https://github.com/xuwei-k</url>
        </developer>
        <developer>
          <id>gakuzzzz</id>
          <name>Manabu Nakamura</name>
          <url>https://github.com/gakuzzzz</url>
        </developer>
        <developer>
          <id>kxbmap</id>
          <name>kxbmap</name>
          <url>https://github.com/kxbmap</url>
        </developer>
        <developer>
          <id>tkawachi</id>
          <name>Takashi Kawachi</name>
          <url>https://github.com/tkawachi</url>
        </developer>
      </developers>

}

