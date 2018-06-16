import java.io.PrintWriter
import java.util.concurrent.TimeUnit

import iccparser._
import org.argus.amandroid.alir.componentSummary._
import org.argus.jawa.core._
import org.argus.jawa.summary.wu.PTStore
import writer.{CsvGraphWriter, DotGraphWriter, SystemOutGraphWriter, XmlGraphWriter}

class Argus {

  var baseResultPath: String = "./output"
  var mode: String = "ACTIVITY"

  def run(apkLocation: String, baseResultPath: String = "./output", mode: String): Unit = {
    this.baseResultPath = baseResultPath
    this.mode = mode
    val startTime = System.nanoTime
    println("Starttime : " + startTime)
    println("Running argus")

    val reporter = new PrintReporter(MsgLevel.ERROR)
    val yard = new ApkYard(reporter)
    val store: PTStore = new PTStore

    val csvWriter: CsvGraphWriter = new CsvGraphWriter(new PrintWriter(""))
    val xmlWriter: XmlGraphWriter = new XmlGraphWriter(new PrintWriter(""))
    val dotWriter: DotGraphWriter = new DotGraphWriter(new PrintWriter(""))
    val outWriter: SystemOutGraphWriter = new SystemOutGraphWriter
    val writers = Set(csvWriter, xmlWriter, dotWriter, outWriter)

    val bottomUpParser = new BottomUpParser(store)

    val apk = bottomUpParser.loadApk(apkLocation, yard, reporter)
    printEstimatedTimeElapsed(startTime)

    bottomUpParser.parse(apk, yard)
    printEstimatedTimeElapsed(startTime)

    bottomUpParser.collectIntents(apk)
    printEstimatedTimeElapsed(startTime)

    bottomUpParser.writeGraph(writers, apk)
    printEstimatedTimeElapsed(startTime)
  }

  def printEstimatedTimeElapsed(startTime: Long): Unit ={
    val estimatedTime = System.nanoTime - startTime
    println("Estimated time elapsed: " + TimeUnit.SECONDS.convert(estimatedTime, TimeUnit.NANOSECONDS))
  }
}
