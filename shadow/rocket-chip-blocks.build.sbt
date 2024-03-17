// build.sbt
scalaVersion := "2.13.10"
val chiselVersion = "5.1.0"
addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies += "org.chipsalliance" %% "chisel" % chiselVersion

// rocket-chip
val rocketChipOut = Glob("../rocket-chip/out/rocketchip/*/assembly.dest/out.jar")
Compile / unmanagedJars += file(fileTreeView.value.list(rocketChipOut).map(_._1.toString).head)

