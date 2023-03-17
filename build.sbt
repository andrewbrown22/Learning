import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "kurtlogan",
      scalaVersion := "2.13.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "adventOfCode2022Take2",
    libraryDependencies += scalaTest % Test
  )
