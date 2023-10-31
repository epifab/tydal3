val scala3Versions = List("3.3.1", "3.3.0", "3.0.1")

lazy val root = project
  .in(file("."))
  .settings(
    name               := "tydal3",
    version            := "0.2",
    scalaVersion       := scala3Versions.head,
    crossScalaVersions := scala3Versions,
    libraryDependencies ++= Seq(
      "org.tpolecat"  %% "skunk-core"  % "0.5.1",
      "org.tpolecat"  %% "skunk-circe" % "0.5.1",
      "org.scalactic" %% "scalactic"   % "3.2.16" % Test,
      "org.scalatest" %% "scalatest"   % "3.2.15" % Test
    )
  )
