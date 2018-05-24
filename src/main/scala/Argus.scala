import java.io.{File, PrintWriter, Writer}
import java.util.concurrent.TimeUnit

import hu.ssh.progressbar.console.ConsoleProgressBar
import iccparser._
import org.argus.amandroid.alir.componentSummary.ComponentSummaryTable.CHANNELS
import org.argus.amandroid.alir.componentSummary._
import org.argus.amandroid.alir.pta.model.AndroidModelCallHandler
import org.argus.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.argus.amandroid.alir.pta.summaryBasedAnalysis.AndroidSummaryProvider
import org.argus.amandroid.core.decompile.{DecompileLayout, DecompileStrategy, DecompilerSettings}
import org.argus.amandroid.core.model.Intent
import org.argus.amandroid.core.{AndroidGlobalConfig, ApkGlobal}
import org.argus.amandroid.summary.wu.IntentWu
import org.argus.jawa.alir.Context
import org.argus.jawa.alir.cfg.ICFGNode
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.alir.pta.PTASlot
import org.argus.jawa.alir.reachability.SignatureBasedCallGraph
import org.argus.jawa.core._
import org.argus.jawa.core.util.{ISet, MList, _}
import org.argus.jawa.summary.wu.{PTStore, PTSummary, WorkUnit}
import org.argus.jawa.summary.{BottomUpSummaryGenerator, SummaryManager}

import scala.concurrent.duration.FiniteDuration

class Argus {

  var baseResultPath: String = "./output"
  var mode: String = "ACTIVITY"

  def run(apkLocation: String, baseResultPath: String = "./output", mode: String): Unit = {
    this.baseResultPath = baseResultPath
    this.mode = mode
    println("Running argus")

    val reporter = new PrintReporter(MsgLevel.ERROR)
    val yard = new ApkYard(reporter)

    val apk: ApkGlobal = loadApk(apkLocation, yard, reporter)

    val parser = new ActivityAppParser(apk, yard)
    val dotGraph = parser.Intent()
    parser.run(dotGraph, new PrintWriter(new File(s"$baseResultPath/${apk.model.getPackageName}-activity.dot")))
    new FullAppParser(apk, yard).run(dotGraph, new PrintWriter(new File(s"$baseResultPath/${apk.model.getPackageName}-full.dot")))
    new SimpleAppParser(apk, yard).run(dotGraph, new PrintWriter(new File(s"$baseResultPath/${apk.model.getPackageName}-simple.dot")))

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
