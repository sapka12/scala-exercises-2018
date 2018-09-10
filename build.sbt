name := "scala-2018"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "com.sksamuel.scrimage" % "scrimage-core_2.12" % "2.1.8"
libraryDependencies += "com.sksamuel.scrimage" % "scrimage-filters_2.12" % "2.1.8"
libraryDependencies += "com.sksamuel.scrimage" % "scrimage-io-extra_2.12" % "2.1.8"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.3.1"
