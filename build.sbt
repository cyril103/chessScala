lazy val root = (project in file("."))
  .settings(
    name := "chessGame",
    scalaVersion := "2.12.4"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"