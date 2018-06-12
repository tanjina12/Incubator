import java.io.{File, PrintWriter}
import java.util.concurrent.TimeUnit

import iccparser._
import org.argus.amandroid.alir.componentSummary._
import org.argus.amandroid.core.decompile.{DecompileLayout, DecompileStrategy, DecompilerSettings}
import org.argus.amandroid.core.{AndroidGlobalConfig, ApkGlobal}
import org.argus.jawa.core._
import org.argus.jawa.core.util._

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

    val apk: ApkGlobal = loadApk(apkLocation, yard, reporter)


    val parser = new ActivityAppParser(apk, yard)
    parser.IntentV2()
    val dotGraph = parser.componentBasedGraph(apk, yard)

    val estimatedTime = System.nanoTime - startTime
    println("Estimated time elapsed: " + TimeUnit.SECONDS.convert(estimatedTime, TimeUnit.NANOSECONDS))
//    parser.run(dotGraph, new PrintWriter(new File(s"$baseResultPath/${apk.model.getPackageName}-activity.dot")))
//    new FullAppParser(apk, yard).run(dotGraph, new PrintWriter(new File(s"$baseResultPath/${apk.model.getPackageName}-full.dot")))
//    new SimpleAppParser(apk, yard).run(dotGraph, new PrintWriter(new File(s"$baseResultPath/${apk.model.getPackageName}-simple.dot")))

  }

  def loadApk(apkLocation: String, yard: ApkYard, reporter: Reporter): ApkGlobal = {
    val apkUri = FileUtil.toUri(apkLocation)
    val outputUri = FileUtil.toUri("./output")
    val layout = DecompileLayout(outputUri, createFolder = true, "src", "lib", createSeparateFolderForDexes = true)
    val strategy = DecompileStrategy(layout, new DefaultLibraryAPISummary(AndroidGlobalConfig.settings.third_party_lib_file))
    val settings = DecompilerSettings(debugMode = false, forceDelete = true, strategy, reporter)
    yard.loadApk(apkUri, settings, collectInfo = true, resolveCallBack = true, guessAppPackages = false)
  }
}
