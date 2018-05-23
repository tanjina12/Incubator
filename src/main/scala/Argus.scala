import java.io.{File, PrintWriter, Writer}
import java.util.concurrent.TimeUnit

import hu.ssh.progressbar.console.ConsoleProgressBar
import iccparser.{ActivityAppParser, BaseAppParser, DotGraphModel, FullAppParser}
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
        if (mode == "Full") {
          print("Running full mode")
          new FullAppParser(apk, yard).run
        } else if (mode == "ACTIVITY") {
          println("Running activity mode")
          val outputLocation = s"$baseResultPath/${apk.model.getPackageName}-activity.dot"
          val writer = new PrintWriter(new File(outputLocation))
          new ActivityAppParser(apk, yard, writer).run
        }

    //    Intent(apk)
//    componentBasedGraph(apk, yard)
  }

  def loadApk(apkLocation: String, yard: ApkYard, reporter: Reporter): ApkGlobal = {
    val apkUri = FileUtil.toUri(apkLocation)
    val outputUri = FileUtil.toUri("./output")
    val layout = DecompileLayout(outputUri, createFolder = true, "src", "lib", createSeparateFolderForDexes = true)
    val strategy = DecompileStrategy(layout, new DefaultLibraryAPISummary(AndroidGlobalConfig.settings.third_party_lib_file))
    val settings = DecompilerSettings(debugMode = false, forceDelete = true, strategy, reporter)
    yard.loadApk(apkUri, settings, collectInfo = true, resolveCallBack = true, guessAppPackages = false)
  }

  def Intent(apk: ApkGlobal): Unit = {
    val handler: AndroidModelCallHandler = new AndroidModelCallHandler
    val sm: SummaryManager = new AndroidSummaryProvider(apk).getSummaryManager
    val analysis = new BottomUpSummaryGenerator[Global](apk, sm, handler,
      PTSummary(_, _),
      ConsoleProgressBar.on(System.out).withFormat("[:bar] :percent% :elapsed Left: :remain"))
    val store: PTStore = new PTStore
    val sigs: ISet[Signature] = apk.model.getComponentInfos.flatMap(apk.getEntryPoints)
    val cg = SignatureBasedCallGraph(apk, sigs, None)
    val orderedWUs: IList[WorkUnit[Global]] = cg.topologicalSort(true).map { sig =>
      val method = apk.getMethodOrResolve(sig).getOrElse(throw new RuntimeException("Method does not exist: " + sig))
      new IntentWu(apk, method, sm, handler, store, "intent")
    }

    analysis.build(orderedWUs)
    val candidate = store.getPropertyOrElse[MSet[(Context, PTASlot)]]("intent", msetEmpty)
    val intents: MSet[(Intent, Signature)] = msetEmpty

    val lol = new DotGraphModel()

    val signature: MMap[Signature, String] = mmapEmpty
    if (!candidate.nonEmpty) {
      return
    }
    candidate.foreach { case (ctx, s) =>
      val intentInss = store.resolved.pointsToSet(ctx, s)
      val intent = IntentHelper.getIntentContents(store.resolved, intentInss, ctx)
      println(s"${ctx.getMethodSig.methodName} calls Intent:")
      println(intent)
      println()
      if (intent.head.componentNames.nonEmpty) {
        signature.put(ctx.getMethodSig, intent.head.componentNames.head)
        intents.add((intent.head, ctx.getMethodSig))
        lol.add(ctx.getMethodSig, intent)
      } else {
        //        println(s"Origin ${ctx.getMethodSig}")
        println(s"NO component link. Its likely an action ${intent}")
      }
    }

    signature.foreach { s =>
      val sig = getSignature(sigs, s._2)
      cg.addCall(s._1, sig)
    }

    //    buildFullAppGraph(cg, apk)
    //        buildSimplifiedAppGraph(cg, apk)
    buildActivityGraph(intents, apk)
  }

  def getSignature(sigs: ISet[Signature], className: String): Signature = {
    val targetSig = sigs.filter(p => p.methodName == "onCreate" && p.getClassName == className)
    if (targetSig.nonEmpty) {
      if (targetSig.size == 1) {
        println("onCreate found")
        return targetSig.head
      } else {
        println("More than one 1 onCreate found!")
      }
    }
    println(s"No onCreate method found! $className")
    new Signature(s"${className.replace(".", "/")};.dummyInit:()V")
  }


  def componentBasedGraph(apk: ApkGlobal, yard: ApkYard): Unit = {
    ComponentBasedAnalysis.prepare(Set(apk))(FiniteDuration(5, TimeUnit.MINUTES))

    val cba = new ComponentBasedAnalysis(yard)
    cba.phase1(Set(apk))
    val iddResult = cba.phase2(Set(apk))

    val l: MSet[(ICFGNode, IntentCaller)] = msetEmpty

    apk.getSummaryTables.foreach { st =>
      val table: ICC_Summary = st._2.get(CHANNELS.ICC)
      table.asCaller.foreach { x =>
        x._2 match {
          case p: IntentCaller => {
            if (p.intent.componentNames.nonEmpty) {
              l.add((x._1, p))
            } else {
              //          l.add(x._1, intent)
              println(s"NO component link. Its likely an action $p")
            }
          }
          case p: IntentCallee =>
          case p: IntentResultCallee =>
          case p: IntentResultCallee =>
        }

      }
    }

    buildActivityGraph(l.map(x => (x._2.intent, x._1.getOwner)), apk)
  }

  def buildFullAppGraph(cg: CallGraph, apk: ApkGlobal): Unit = {
    val t: MList[(String, String)] = mlistEmpty

    cg.getCallMap.foreach { c =>
      t += ((c._1.getClassName, c._1.toString()))
      c._2.foreach { p =>
        if (c._1.getClassName != p.getClassName) {
          if (p.classTyp.getPackageName == apk.model.getPackageName) {
            t += ((c._1.toString(), p.signature.toString))
          }
        }
      }
    }

    val writer = new PrintWriter(new File(s"$baseResultPath/${apk.model.getPackageName}-full.dot"))

    println("digraph G {")
    writer.write("digraph G {")
    t.foreach { x =>
      println("\"" + x._1 + "\" -> \"" + x._2 + "\"")
      writer.write("\"" + x._1 + "\" -> \"" + x._2 + "\"")
    }
    println("}")
    writer.write("}")
    writer.close()
  }

  def buildSimplifiedAppGraph(cg: CallGraph, apk: ApkGlobal): Unit = {
    val t: MSet[(String, String, String)] = msetEmpty

    cg.getCallMap.foreach { c =>
      c._2.foreach { p =>
        val source = c._1.getClassName.replaceAll("[$].*", "")
        val target = p.getClassName.replaceFirst("[$].*", "")
        if (source != target) {
          if (p.classTyp.getPackageName == apk.model.getPackageName || p.methodName == "dummyInit") {
            t += ((source, target, c._1.methodName))
          }
        }
      }
    }

    val writer = new PrintWriter(new File(s"$baseResultPath/${apk.model.getPackageName}-simple.dot"))

    println("digraph G {")
    writer.write("digraph G {")
    t.foreach { x =>
      println("\"" + x._1 + "\" -> \"" + x._2 + "\"[label=" + x._3 + "]")
      writer.write("\"" + x._1 + "\" -> \"" + x._2 + "\"[label=" + x._3 + "]")
    }
    apk.model.getActivities.foreach {
      x =>
        println("\"" + x.name + "\"")
        writer.write("\"" + x.name + "\"")
    }
    println("}")
    writer.write("}")
    writer.close()
  }

  def buildActivityGraph(intents: MSet[(Intent, Signature)], apk: ApkGlobal): Unit = {

    val t: MSet[(String, String, String)] = msetEmpty

    intents.foreach { x =>
      var target = ""
      if (x._1.componentNames.nonEmpty) {
        target = x._1.componentNames.head
      } else {
        target = x._1.actions.head
      }
      val source = x._2.getClassName
      val label = x._2.methodName
      t.add(source, target, label)
    }

    val writer = new PrintWriter(new File(s"$baseResultPath/${apk.model.getPackageName}-activity.dot"))

    println("digraph G {")
    writer.write("digraph G {")
    t.foreach { x =>
      println("\"" + x._1 + "\" -> \"" + x._2 + "\"[label=" + x._3 + "]")
      writer.write("\"" + x._1 + "\" -> \"" + x._2 + "\"[label=" + x._3 + "]")
    }
    apk.model.getActivities.foreach {
      x =>
        println("\"" + x.name + "\"")
        writer.write("\"" + x.name + "\"")
    }
    println("}")
    writer.write("}")
    writer.close()
  }
}
