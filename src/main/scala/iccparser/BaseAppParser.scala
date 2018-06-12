package iccparser

import java.io.Writer
import java.util.concurrent.TimeUnit

import hu.ssh.progressbar.console.ConsoleProgressBar
import org.argus.amandroid.alir.componentSummary.ComponentSummaryTable.CHANNELS
import org.argus.amandroid.alir.componentSummary._
import org.argus.amandroid.alir.pta.model.AndroidModelCallHandler
import org.argus.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.argus.amandroid.alir.pta.summaryBasedAnalysis.AndroidSummaryProvider
import org.argus.amandroid.core.model.Intent
import org.argus.amandroid.core.{AndroidConstants, ApkGlobal}
import org.argus.amandroid.summary.wu.IntentWu
import org.argus.jawa.alir.Context
import org.argus.jawa.alir.cfg.ICFGNode
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.alir.pta.PTASlot
import org.argus.jawa.alir.pta.suspark.InterProceduralSuperSpark
import org.argus.jawa.alir.reachability.SignatureBasedCallGraph
import org.argus.jawa.core.util._
import org.argus.jawa.core.{Global, Signature}
import org.argus.jawa.summary.wu.{PTStore, PTSummary, WorkUnit}
import org.argus.jawa.summary.{BottomUpSummaryGenerator, SummaryManager}

import scala.concurrent.duration.FiniteDuration

abstract class BaseAppParser(apk: ApkGlobal, yard: ApkYard, writer: Writer) {

  def run: Unit

  def run(dotGraphModel: DotGraphModel, writer: Writer): Unit

  val lineSeparator = "\r\n"

  def IntentV2() = {
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
//    val candidates = store.getPropertyOrElse[MSet[(Context, PTASlot)]]("intent", msetEmpty)
    val dotGraph = new DotGraphModel(cg)

    val explicitIntents: MSet[(Context, Intent)] = msetEmpty
    val implicitIntents: MSet[(Context, Intent)] = msetEmpty

    candidate.foreach { case (ctx, s) =>
      val intentInss = store.resolved.pointsToSet(ctx, s)
       val intent = IntentHelper.getIntentContents(store.resolved, intentInss, ctx)

      intent.foreach(u => {
//        println("Found intent: " + intent)
        explicitIntents.add(ctx, u)

      })
    }

    println()

    explicitIntents.foreach {
      case (context, intent) =>
        if (intent.componentNames.nonEmpty) {
          intent.componentNames.foreach(name => {
            println(context.getMethodSig.classTyp + " -ICC-> " + name + " by method " + context.getMethodSig.methodName)
          })
        }
        if (intent.actions.nonEmpty) {
          intent.actions.foreach(f => {
            val p = apk.model.getIntentFilterDB.getIntentFmap
            p.foreach {
              case (classType, intentFilters) =>
                intentFilters.foreach(filter => {
                  if (filter.hasAction(f)) {
                    filter.getActions.foreach(u => {
                      if (u.equals(f)){
                        println(context.getMethodSig.classTyp + " -ICC-Action-> " + f + " --> " + classType + " by method " + context.getMethodSig.methodName)
                      }
                    })
                  }
                })
            }
          })
        }

    }

//    dotGraph
  }

  def Intent(): DotGraphModel = {
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
    val dotGraph = new DotGraphModel(cg)

    candidate.foreach { case (ctx, s) =>
      val intentInss = store.resolved.pointsToSet(ctx, s)
      val intent = IntentHelper.getIntentContents(store.resolved, intentInss, ctx)
      println(s"${ctx.getMethodSig.methodName} calls Intent:")
      println(intent)
      println()

      if (intent.nonEmpty) {
        if (intent.head.componentNames.nonEmpty) {
          dotGraph.add(ctx.getMethodSig, intent, caller = true)
        } else {
          println(s"NO component link. Its likely an action ${intent.head}")
        }
      } else {
        println(s"No intent found... $ctx")
      }
    }

    dotGraph
  }

  def Spark(): CallGraph = {
    val entryPoints = apk.getEntryPoints(AndroidConstants.MAINCOMP_ENV) // Exposed components
    val spark = new InterProceduralSuperSpark(apk)
    val idfg = spark.build(entryPoints.map(_.getSignature))
    val icfg = idfg.icfg
    return icfg.getCallGraph
  }

  def Signature(): CallGraph = {
    val entryPoints = apk.getEntryPoints(AndroidConstants.MAINCOMP_ENV)
    return SignatureBasedCallGraph(apk, entryPoints.map(_.getSignature), None)
  }

  def componentBasedGraph(apk: ApkGlobal, yard: ApkYard): DotGraphModel = {
    ComponentBasedAnalysis.prepare(Set(apk))(FiniteDuration(5, TimeUnit.MINUTES))
    val cba = new ComponentBasedAnalysis(yard)
    cba.phase1(Set(apk))
    val iddResult = cba.phase2(Set(apk))

    val l: MSet[(ICFGNode, IntentCaller)] = msetEmpty

    val dotGraphModel = new DotGraphModel()

    val callees: MSet[IntentCallee] = msetEmpty
    val callers: MSet[(ICFGNode, IntentCaller)] = msetEmpty

    apk.getSummaryTables.foreach { st =>
      val table: ICC_Summary = st._2.get(CHANNELS.ICC)
      table.asCallee.foreach { x =>
        x._2 match {
          case intent: IntentCaller => println(s"AsCallee: IntentCaller found! $intent")
          case intent: IntentCallee => {
            println(s"AsCallee: IntentCallee found! $intent")
            if (intent.exported) {
              callees.add(intent)
            }
          }
          case intent: IntentResultCaller => println(s"AsCallee: IntentResultCaller found! $intent")
          case intent: IntentResultCallee => println(s"AsCallee: IntentResultCallee found! $intent")
        }
      }
      table.asCaller.foreach { x =>
        x._2 match {
          case intent: IntentCaller => {
            println(s"AsCaller: IntentCaller found! $intent")
            callers.add(x._1, intent)
          }
          case intent: IntentCallee => println(s"AsCaller: IntentCallee found! $intent")
          case intent: IntentResultCaller => println(s"AsCaller: IntentResultCaller found! $intent")
          case intent: IntentResultCallee => println(s"AsCaller: IntentResultCallee found! $intent")
        }
      }

    }
    println()
    callers.foreach { case (node, intent) =>
      val comp = intent.component
      val compType = intent.compTyp
      if (intent.intent.componentNames.nonEmpty) {
        intent.intent.componentNames.foreach(name => {
          println(comp + " -ICC-> " + name + " by method " + node.getOwner.methodName)
        })
      }
      if (intent.intent.actions.nonEmpty) {
        intent.intent.actions.foreach(action => {
          callees.foreach(callee => {
            callee.filter.foreach(filter => {
              if (filter.getActions.contains(action)) {
                println(comp + " -ICC-Action-> " + action + " --> " + callee.component + " by method " + node.getOwner.methodName)
              }
            })
          })
        })
      }
    }


    dotGraphModel
  }
}
