import java.io.PrintWriter
import java.util.concurrent.TimeUnit

import iccparser._
import org.argus.amandroid.alir.componentSummary._
import org.argus.jawa.core._
import org.argus.jawa.summary.wu.PTStore
import writer._

class Argus {

  var baseResultPath: String = "./output"
  var parseInnerClassToOuterClass: Boolean = true

  def run(apkLocation: String, baseResultPath: String = "./output"): Unit = {
    this.baseResultPath = baseResultPath
    val startTime = System.nanoTime
    println("Starttime : " + startTime)
    println("Running argus")

    val reporter = new PrintReporter(MsgLevel.ERROR)
    val yard = new ApkYard(reporter)
    val store: PTStore = new PTStore
    val parser = new BottomUpParser(store)
//    val parser = new ComponentBasedParser

    println("Load apk")
    val apk = parser.loadApk(apkLocation, yard, reporter)
    printEstimatedTimeElapsed(startTime)

    println("Parse apk")
    parser.parse(apk, yard)
    printEstimatedTimeElapsed(startTime)

    println("Collect intents")
    parser.collectIntents(apk)
    printEstimatedTimeElapsed(startTime)


    parser.bindWidgetsToIccMethods(apk, reporter)


    println("Write graph to file")
    val csvWriter: CsvGraphWriter = new CsvGraphWriter(new PrintWriter(s"$baseResultPath/${apk.model.getPackageName}.csv"))
    val xmlWriter: XmlGraphWriter = new XmlGraphWriter(new PrintWriter(s"$baseResultPath/${apk.model.getPackageName}.xml"))
    val dotWriter: DotGraphWriter = new DotGraphWriter(new PrintWriter(s"$baseResultPath/${apk.model.getPackageName}.dot"))
    val outWriter: SystemOutGraphWriter = new SystemOutGraphWriter
    val methodWriter: MethodWriter = new MethodWriter(new PrintWriter(s"$baseResultPath/${apk.model.getPackageName}-methods.csv"))
    val writers = Set(csvWriter, xmlWriter, dotWriter, outWriter)

    parser.writeGraph(writers, apk)
    printEstimatedTimeElapsed(startTime)

    println("Write methods")
    parser.writeMethods(methodWriter, apk)
    printEstimatedTimeElapsed(startTime)

    println("Write transitive graph to file")
    val tcsvWriter: CsvGraphWriter = new CsvGraphWriter(new PrintWriter(s"$baseResultPath/${apk.model.getPackageName}-transitive.csv"))
    val txmlWriter: XmlGraphWriter = new XmlGraphWriter(new PrintWriter(s"$baseResultPath/${apk.model.getPackageName}-transitive.xml"))
    val tdotWriter: DotGraphWriter = new DotGraphWriter(new PrintWriter(s"$baseResultPath/${apk.model.getPackageName}-transitive.dot"))
    val twriters = Set(tcsvWriter, txmlWriter, tdotWriter)
    parser.writeTransitiveGraph(twriters, apk)

    println(s"Finished analysis for ${apk.model.getPackageName}")
  }


  def printEstimatedTimeElapsed(startTime: Long): Unit = {
    val estimatedTime = System.nanoTime - startTime
    println("Estimated time elapsed: " + TimeUnit.SECONDS.convert(estimatedTime, TimeUnit.NANOSECONDS))
  }
}
