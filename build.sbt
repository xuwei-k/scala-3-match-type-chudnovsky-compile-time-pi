val common = Def.settings(
  scalaVersion := "3.8.2"
)

common

val core = project.settings(common)

val chudnovsky = project.settings(common).dependsOn(core)
