scalaVersion := "3.8.4"

val core = project

val chudnovsky = project.dependsOn(core)
