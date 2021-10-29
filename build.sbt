val scala3Version = "3.0.1-RC2"
resolvers += "jitpack" at "https://jitpack.io"

lazy val root = project
  .in(file("."))
  .settings(
    name := "tydal3",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.tpolecat" %% "skunk-core" % "0.2.0",
      "org.tpolecat" %% "skunk-circe" % "0.2.0",
      "org.scalactic" %% "scalactic" % "3.2.9" % Test,
      "org.scalatest" %% "scalatest" % "3.2.9" % Test
    )
  )
