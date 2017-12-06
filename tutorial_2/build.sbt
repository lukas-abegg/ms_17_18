import AssemblyKeys._ // put this at the top of the file

assemblySettings

name := "tutorial_2"

version := "0.1"

scalaVersion := "2.12.1"

mainClass in (Compile, run) := Some("tutorial_2.Main")

mainClass in (Compile, packageBin) := Some("tutorial_2.Main")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.16"
libraryDependencies += "io.suzaku" %% "boopickle" % "1.2.6"