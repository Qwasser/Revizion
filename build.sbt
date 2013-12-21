name := "Revizions"

version := "0.1"


resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.1" % "test",
  "org.specs2" %% "specs2" % "2.3.4" % "test",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

