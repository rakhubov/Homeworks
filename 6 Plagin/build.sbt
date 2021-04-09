import sbt._

name := "Homework 6 Plagin"
version := "0.1"
scalaVersion := "2.13.4"
sbtPlugin := true

val bulkySources = taskKey[Seq[(Int, File)]](
  "Show rows more bulkyThresholdInLines in descending order"
)
val bulkyThresholdInLines = settingKey[Int]("The minimum size of the row")

lazy val root = (project in file("."))
  .settings(
    bulkyThresholdInLines := 100,
    Compile / bulkySources := getBulkySources(
      (sources in Compile).value,
      bulkyThresholdInLines.value
    ),
    Test / bulkySources := getBulkySources(
      (sources in Test).value,
      bulkyThresholdInLines.value
    )
  )

def getBulkySources(files: Seq[File], minSize: Int): Seq[(Int, File)] = {
  files
    .map(file => (sbt.IO.readLines(file).filterNot(_.isEmpty).size, file))
    .sortBy { case (x, _) => x }
    .reverse
    .filter { case (x, _) => x >= minSize }
}
