val scala3Versions = List("3.0.1", "3.0.2")

lazy val root = project
  .in(file("."))
  .settings(
    name := "tydal3",
    version := "0.2",

    scalaVersion := scala3Versions.head,
    crossScalaVersions := scala3Versions,

    libraryDependencies ++= Seq(
      "org.tpolecat" %% "skunk-core" % "0.2.2",
      "org.tpolecat" %% "skunk-circe" % "0.2.2",
      "org.scalactic" %% "scalactic" % "3.2.9" % Test,
      "org.scalatest" %% "scalatest" % "3.2.9" % Test
    )
  )
