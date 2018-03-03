import Dependencies._

lazy val cryptopals = (project in file(".")).
    settings(
        inThisBuild(List(
            organization := "de.musmehl",
            scalaVersion := "2.12.4",
            version := "0.1.0-SNAPSHOT"
        )),
        name := "cryptopals",
        scalastyleConfig := file("scalastyle_config.xml"),
        libraryDependencies ++= Seq(
            scalaTest,
            scalaCheck
        )
    )
