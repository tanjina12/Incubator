import java.io.{File, PrintWriter}
import java.nio.channels.Channels
import java.util.concurrent.TimeUnit

import hu.ssh.progressbar.console.ConsoleProgressBar
import org.argus.amandroid.alir.componentSummary.ComponentSummaryTable.CHANNELS
import org.argus.amandroid.alir.componentSummary._
import org.argus.amandroid.alir.dataRecorder.{DataCollector, MetricRepo}
import org.argus.amandroid.alir.pta.model.AndroidModelCallHandler
import org.argus.amandroid.alir.pta.reachingFactsAnalysis.{AndroidReachingFactsAnalysis, AndroidReachingFactsAnalysisConfig, IntentHelper}
import org.argus.amandroid.alir.pta.summaryBasedAnalysis.AndroidSummaryProvider
import org.argus.amandroid.core.decompile.{DecompileLayout, DecompileStrategy, DecompilerSettings}
import org.argus.amandroid.core.model.Intent
import org.argus.amandroid.core.{AndroidConstants, AndroidGlobalConfig, ApkGlobal}
import org.argus.amandroid.plugin.communication.CommunicationSourceAndSinkManager
import org.argus.amandroid.summary.wu.IntentWu
import org.argus.jawa.alir.Context
import org.argus.jawa.alir.cfg.{ICFGNode, InterProceduralControlFlowGraph}
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.alir.dda.InterProceduralDataDependenceAnalysis
import org.argus.jawa.alir.pta.suspark.InterProceduralSuperSpark
import org.argus.jawa.alir.pta.{PTAResult, PTASlot}
import org.argus.jawa.alir.reachability.SignatureBasedCallGraph
import org.argus.jawa.core._
import org.argus.jawa.core.util.{MList, _}
import org.argus.jawa.summary.wu.{PTStore, PTSummary, WorkUnit}
import org.argus.jawa.summary.{BottomUpSummaryGenerator, SummaryManager}

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

class Argus {

  def run(): Unit = {
    println("Running argus in Scala")
//    val apkUri = FileUtil.toUri("app-debug.apk")
    //        val apkUri = FileUtil.toUri("aarddict.android_26.apk")
    //    val apkUri = FileUtil.toUri("ab.nwdesings.it.autocentribalduina.apk")
            val apkUri = FileUtil.toUri("ab.java.programming.apk")
    //        val apkUri = FileUtil.toUri("abid.hafedh.health.babygrowthguide.apk")
    val outputUri = FileUtil.toUri("./output")
    val reporter = new PrintReporter(MsgLevel.ERROR)
    val yard = new ApkYard(reporter)
    val layout = DecompileLayout(outputUri, true, "src", "lib", true)
    val strategy = DecompileStrategy(layout, new DefaultLibraryAPISummary(AndroidGlobalConfig.settings.third_party_lib_file))

    val settings = DecompilerSettings(false, true, strategy, reporter)

    val apk: ApkGlobal = yard.loadApk(apkUri, settings, true, true, false)

    //    Spark(apk)
    //    Signature(apk)
    //        Facts(apk)
//        Intent(apk)
    componentBasedGraph(apk, yard)
  }


  def Spark(apk: ApkGlobal): Unit = {
    var entryPoints = apk.getEntryPoints(AndroidConstants.MAINCOMP_ENV) // Exposed components
    val spark = new InterProceduralSuperSpark(apk)
    val idfg = spark.build(entryPoints.map(_.getSignature))
    val icfg = idfg.icfg
    val callGraph = icfg.getCallGraph

    icfg.toDot(new PrintWriter(System.out))


    val x = idfg.ptaresult.pointsToMap
  }

  def Signature(apk: ApkGlobal): Unit = {
    var entryPoints = apk.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
    val signature = SignatureBasedCallGraph(apk, entryPoints.map(_.getSignature), None)
    val callMap = signature.getCallMap
  }

  def Facts(apk: ApkGlobal): Unit = {
    val icfg = new InterProceduralControlFlowGraph[ICFGNode]
    val ptaresult = new PTAResult
    val sp = new AndroidSummaryProvider(apk)

    AndroidReachingFactsAnalysisConfig.resolve_icc = true
    AndroidReachingFactsAnalysisConfig.resolve_static_init = false
    AndroidReachingFactsAnalysisConfig.parallel = false

    val analysis = new AndroidReachingFactsAnalysis(apk, icfg, ptaresult,
      new AndroidModelCallHandler,
      sp.getSummaryManager,
      new ClassLoadManager,
      AndroidReachingFactsAnalysisConfig.resolve_static_init,
      timeout = Some(new MyTimeout(FiniteDuration(5, TimeUnit.MINUTES)))
    )

    var entryPoints = apk.getEntryPoints(AndroidConstants.MAINCOMP_ENV)

    val entryPoint: JawaMethod = entryPoints.head
    val idfg = analysis.build(entryPoint, initContext = new Context(apk.nameUri))
    val iddResult = InterProceduralDataDependenceAnalysis(apk, idfg)
    println(iddResult)

    idfg.icfg.toDot(new PrintWriter(System.out))
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


    val signature: MMap[Signature, String] = mmapEmpty
    candidate.foreach { case (ctx, s) =>
      val intentInss = store.resolved.pointsToSet(ctx, s)
      val intent = IntentHelper.getIntentContents(store.resolved, intentInss, ctx)
      println(s"${ctx.getMethodSig.methodName} calls Intent:")
      println(intent)
      println()
      if (intent.head.componentNames.nonEmpty) {
        signature.put(ctx.getMethodSig, intent.head.componentNames.head)
        intents.add((intent.head, ctx.getMethodSig))
      } else {
        println(s"NO component link. Its likely an action ${intent}")
      }
    }

    signature.foreach { s =>
      val sig = getSignature(sigs, s._2)
      cg.addCall(s._1, sig)
    }

    //    buildFullAppGraph(cg, apk)
    //    buildSimplifiedAppGraph(cg, apk)
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
        val intent: IntentCaller = x._2.asInstanceOf[IntentCaller]
        if (intent.intent.componentNames.nonEmpty) {
          l.add((x._1, intent))
        } else {
          println(s"NO component link. Its likely an action ${intent}")
        }
      }
    }

    apk.model.getCallbackMethods

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

    val writer = new PrintWriter(new File("data.dot"))

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

    val writer = new PrintWriter(new File("data.dot"))

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
      val target = x._1.componentNames.head
      val source = x._2.getClassName
      val label = x._2.methodName
      t.add(source, target, label)
    }

    val writer = new PrintWriter(new File("data.dot"))

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
