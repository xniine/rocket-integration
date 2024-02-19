// build.sbt
scalaVersion := "2.13.10"
val chiselVersion = "5.1.0"
addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies += "org.chipsalliance" %% "chisel" % chiselVersion

// rocket-chip
val rocketChipOut = Glob("../rocket-chip/out/rocketchip/*/assembly.dest/out.jar")
Compile / unmanagedJars += file(fileTreeView.value.list(rocketChipOut).map(_._1.toString).head)

// sifive-blocks
val sifiveBlocksOut = Glob("../rocket-chip-blocks/target/*/*.jar")
Compile / unmanagedJars += file(fileTreeView.value.list(sifiveBlocksOut).map(_._1.toString).head)

// sifive-cache
val sifiveCacheOut = Glob("../rocket-chip-inclusive-cache/target/*/*.jar")
Compile / unmanagedJars += file(fileTreeView.value.list(sifiveCacheOut).map(_._1.toString).head)

