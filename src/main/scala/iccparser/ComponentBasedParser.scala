package iccparser

import java.util.concurrent.TimeUnit

import org.argus.amandroid.alir.componentSummary.ComponentSummaryTable.CHANNELS
import org.argus.amandroid.alir.componentSummary._
import org.argus.amandroid.alir.taintAnalysis.AndroidSourceAndSinkManager
import org.argus.amandroid.core.{AndroidConstants, AndroidGlobalConfig, ApkGlobal}
import org.argus.jawa.alir.cfg.ICFGNode
import org.argus.jawa.alir.util.ExplicitValueFinder
import org.argus.jawa.ast.{CallStatement, Location}
import org.argus.jawa.core.{MsgLevel, PrintReporter, Signature}
import org.argus.jawa.core.util.{MSet, msetEmpty}

import scala.concurrent.duration.FiniteDuration



class ComponentBasedParser extends BaseAppParser {

  def parse(apk: ApkGlobal, yard: ApkYard) = {
    ComponentBasedAnalysis.prepare(Set(apk))(FiniteDuration(5, TimeUnit.MINUTES))
    val componentBasedAnalysis = new ComponentBasedAnalysis(yard)
    componentBasedAnalysis.phase1(Set(apk))
    val iddResult = componentBasedAnalysis.phase2(Set(apk))
    print()
//    val ssm = new WidgetAndCallBackManager(reporter = new PrintReporter(MsgLevel.ERROR))
//    val x = componentBasedAnalysis.phase3(iddResult, ssm)
    print()
  }


  def collectIntents(apk: ApkGlobal): Unit = {
    val callees: MSet[IntentCallee] = msetEmpty
    val callers: MSet[(ICFGNode, IntentCaller)] = msetEmpty

    apk.getSummaryTables.foreach { st =>
      val table: ICC_Summary = st._2.get(CHANNELS.ICC)
      table.asCallee.foreach { x =>
        x._2 match {
          case intent: IntentCaller => println(s"AsCallee: IntentCaller found! $intent")
          case intent: IntentCallee =>
            println(s"AsCallee: IntentCallee found! $intent")
            if (intent.exported) {
              callees.add(intent)
            }
          case intent: IntentResultCaller => println(s"AsCallee: IntentResultCaller found! $intent")
          case intent: IntentResultCallee => println(s"AsCallee: IntentResultCallee found! $intent")
        }
      }
      table.asCaller.foreach { x =>
        x._2 match {
          case intent: IntentCaller =>
            println(s"AsCaller: IntentCaller found! $intent")
            callers.add(x._1, intent)
          case intent: IntentCallee => println(s"AsCaller: IntentCallee found! $intent")
          case intent: IntentResultCaller => println(s"AsCaller: IntentResultCaller found! $intent")
          case intent: IntentResultCallee => println(s"AsCaller: IntentResultCallee found! $intent")
        }
      }

    }
    println()
    callers.foreach { case (node, intent) =>
      val comp = intent.component
      //      val compType = intent.compTyp
      if (intent.intent.componentNames.nonEmpty) {
        intent.intent.componentNames.foreach(name => {
          println(comp + " -ICC-> " + name + " by method " + node.getOwner.methodName)
          graph = graph :+ ((comp.typ.jawaName, name, node.getOwner.methodName))
        })
      }
      if (intent.intent.actions.nonEmpty) {
        intent.intent.actions.foreach(action => {
          callees.foreach(callee => {
            callee.filter.foreach(filter => {
              if (filter.getActions.contains(action)) {
                println(comp + " -ICC-Action-> " + action + " --> " + callee.component + " by method " + node.getOwner.methodName)
                graph = graph :+ ((comp.typ.jawaName, callee.component.typ.jawaName, node.getOwner.methodName))
              }
            })
          })
        })
      }
    }
  }
}
