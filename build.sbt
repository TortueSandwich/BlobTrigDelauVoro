scalaVersion := "2.13.12"

name := "hello-world"
organization := "ch.epfl.scala"
version := "1.0"

// Dependencies for ScalaFX
libraryDependencies += "org.scalafx" %% "scalafx" % "16.0.0-R24"

// Dependencies for JavaFX
libraryDependencies ++= {
  lazy val osName = System.getProperty("os.name").toLowerCase match {
    case n if n.contains("linux") => "linux"
    case n if n.contains("mac") => "mac"
    case n if n.contains("windows") => "win"
    case _ => throw new Exception("Unknown platform!")
  }
  Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
    .map(m => "org.openjfx" % s"javafx-$m" % "16" classifier osName)
}

lazy val root = (project in file("."))

lazy val runUiMain = taskKey[Unit]("Runs the UiMain class")
runUiMain := (Compile / runMain).toTask(" UiMain").value

lazy val runTestGeom = taskKey[Unit]("Runs the TestGeom class")
runTestGeom := (Compile / runMain).toTask(" TestGeom").value
