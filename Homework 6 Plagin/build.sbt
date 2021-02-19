import sbt._
import sbt.Keys.sources
import sbt.{AutoPlugin, Compile, File, Setting, Test, settingKey, taskKey}
name := "Homework 6 Plagin"
version := "0.1"
scalaVersion := "2.13.4"
sbtPlugin := true

val bulkySources = taskKey[Seq[(Int, File)]](
  "Show rows more bulkyThresholdInLines in descending order"
)
val bulkyThresholdInLines = settingKey[Int]("The minimum size of the row")

def getBulkySources(files: Seq[File], lineLimit: Int): Seq[(Int, File)] = {
  files
    .map(f => (sbt.IO.readLines(f).filterNot(_.isEmpty).size, f))
    .sortBy { case (linesCount, _) => linesCount }
    .reverse
    .filterNot { case (linesCount, _) => linesCount < lineLimit }
}

lazy val root = (project in file("."))
  .settings(
    bulkyThresholdInLines := 100
  )

val projectSettings = Seq(
  Compile / bulkySources := getBulkySources(
    (sources in Compile).value,
    bulkyThresholdInLines.value
  ),
  Test / bulkySources := getBulkySources(
    (sources in Test).value,
    bulkyThresholdInLines.value
  )
)
