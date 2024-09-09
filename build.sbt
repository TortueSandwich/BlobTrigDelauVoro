scalaVersion := "2.13.12"

name := "QuadEdgeTriangulation"
organization := "blob"
version := "1.0"

githubOwner := "TortueSandwich"
githubRepository := "BlobTrigDelauVoro"
githubTokenSource := TokenSource.GitConfig("github.token")

// scalacOptions ++= Seq("-deprecation")


// Dependencies for ScalaFX
libraryDependencies += "org.scalafx" %% "scalafx" % "21.0.0-R32"

// libraryDependencies += "blob" % "quadedgetriangulation_2.13" % "1.0"

lazy val runUiMain = taskKey[Unit]("Runs the UiMain class")
runUiMain := (Compile / runMain).toTask(" UiMain").value

lazy val runTestGeom = taskKey[Unit]("Runs the TestGeom class")
runTestGeom := (Compile / runMain).toTask(" TestGeom").value

lazy val runBench = taskKey[Unit]("Runs delaunay benchmark")
runBench := (Compile / runMain).toTask(" DelaunayBenchmark").value
