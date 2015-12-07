name := "CalendarScript"

version := "1.2"

scalaVersion := "2.11.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "org.scalafx" % "scalafx_2.11" % "8.0.5-R5"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
libraryDependencies += "org.mnode.ical4j" % "ical4j" % "2.0-beta1"
