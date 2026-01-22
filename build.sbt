val common = Def.settings(
  scalaVersion := "3.8.1"
)

common

val core = project.settings(common)

val chudnovsky = project.settings(common).dependsOn(core)
