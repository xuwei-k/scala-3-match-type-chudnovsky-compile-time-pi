scalaVersion := "3.8.3"

val core = project

val chudnovsky = project.dependsOn(core)
