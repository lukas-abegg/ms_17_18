import AssemblyKeys._ // put this at the top of the file

assemblySettings

name := "tutorial_1"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.16"

mainClass in (Compile, run) := Some("tutorial_1.Main")

mainClass in (Compile, packageBin) := Some("tutorial_1.Main")

