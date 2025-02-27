lazy val root = Project(id = "functional-patterns", base = file("."))
  .settings(moduleName := "functional-patterns")
  .aggregate(json, codecs, parsecs, gen, language, typeclasses)
  .settings(
    publish         := {},
    publishLocal    := {},
    publishArtifact := false,
  )

lazy val json    = project.dependsOn(parsecs)
lazy val codecs  = project.dependsOn(json)
lazy val parsecs = project
lazy val gen     = project
lazy val language = project.dependsOn(parsecs)
lazy val typeclasses = project
logLevel := Level.Error