name := "shameless"

version := "0.1"

scalaVersion := "2.12.7"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

scalacOptions += "-Ypartial-unification"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
