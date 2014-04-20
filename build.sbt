organization := "me.less"

name := "harp"

version := "0.1.0-SNAPSHOT"

crossScalaVersions := Seq("2.9.3", "2.10.4", "2.11.0")

scalaVersion := crossScalaVersions.value.head

libraryDependencies ++=
  (if (scalaVersion.value.startsWith("2.11")) Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1")
   else Seq.empty[ModuleID])
