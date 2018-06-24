package iccparser

import hu.ssh.progressbar.console.ConsoleProgressBar
import org.argus.amandroid.alir.componentSummary.ApkYard
import org.argus.amandroid.alir.pta.model.AndroidModelCallHandler
import org.argus.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.argus.amandroid.alir.pta.summaryBasedAnalysis.AndroidSummaryProvider
import org.argus.amandroid.alir.taintAnalysis.AndroidSourceAndSinkManager
import org.argus.amandroid.core.{AndroidConstants, AndroidGlobalConfig, ApkGlobal}
import org.argus.amandroid.core.model.Intent
import org.argus.amandroid.summary.wu.IntentWu
import org.argus.jawa.alir.Context
import org.argus.jawa.alir.pta.PTASlot
import org.argus.jawa.alir.reachability.{ReachabilityAnalysis, SignatureBasedCallGraph}
import org.argus.jawa.alir.util.ExplicitValueFinder
import org.argus.jawa.ast.{CallStatement, Location}
import org.argus.jawa.core.util._
import org.argus.jawa.core._
import org.argus.jawa.summary.store.TaintStore
import org.argus.jawa.summary.taint.BottomUpTaintAnalysis
import org.argus.jawa.summary.wu._
import org.argus.jawa.summary.{BottomUpSummaryGenerator, SummaryManager}
import writer.MethodWriter

class LolManager(sasFilePath: String, iets: MSet[Signature]) extends AndroidSourceAndSinkManager(sasFilePath) {
  private final val TITLE = "PasswordSourceAndSinkManager"

  override def isUISource(apk: ApkGlobal, calleeSig: Signature, callerSig: Signature, callerLoc: Location): Boolean = {
    if (calleeSig.signature == AndroidConstants.ACTIVITY_FINDVIEWBYID || calleeSig.signature == AndroidConstants.VIEW_FINDVIEWBYID
      || calleeSig.signature == "Landroid/view/View;.setOnClickListener:(Landroid/view/View$OnClickListener;)V"
      || callerSig.signature == "Lcom/example/sg10/testcgapp/MainActivity;.gotoScreenTwo:()V"
      || apk.model.getCallbackMethods.contains(callerSig)) {

      val calleeMethod = apk.getMethod(calleeSig).get

      //Als we een setOnClickListener vinden. Dan moeten we in de graph kijken welke class dat initialiseerd aka MainActivity$1
      //Alle intent methods in MainActivity$1 worden dus geactiveerd door die setOnClickListener

      val callerMethod = apk.getMethod(callerSig).get
      val cs = callerLoc.statement.asInstanceOf[CallStatement]
      val nums = ExplicitValueFinder.findExplicitLiteralForArgs(callerMethod, callerLoc, cs.arg(0))
      nums.filter(_.isInt).foreach { num =>
        apk.model.getLayoutControls.get(num.getInt) match {
          case Some(control) =>
            //            return control.isSensitive
            return true
          case None =>
            apk.reporter.error(TITLE, "Layout control with ID " + num + " not found.")
        }
      }
    }
    false
  }
}

class BottomUpParser(store: PTStore) extends BaseAppParser {

  var iets: MSet[Signature] = msetEmpty

  def writeMethods(writer: MethodWriter, apk: ApkGlobal) = {
    val components = collectComponents(apk)
    val methods = collectMethods(apk, components)
    writer.write(methods)
  }

  def parse2(apk: ApkGlobal, yard: ApkYard): Unit = {
    val ssm = new LolManager(AndroidGlobalConfig.settings.sas_file, iets)
    val reporter = new PrintReporter(MsgLevel.INFO)

    val ta = new BottomUpTaintAnalysis[ApkGlobal](apk, new AndroidSummaryProvider(apk), new AndroidModelCallHandler, ssm, reporter)

    val eps = apk.model.getEnvMap.map(_._2._1).toSet
    val taintMap = ta.process(eps)
    print()

    val haha: MMap[Signature, MSet[Signature]] = mmapEmpty
    iets.foreach(i => {
      if (apk.model.getCallbackMethods.contains(i)) {
        println("Found callback method in .... " + i.signature)
      } else {
        val t = apk.model.getCallbackMethods.map(x => x)
        val cg = SignatureBasedCallGraph(apk, t)
        t.foreach(s => {
          val reachables = cg.getReachableMethods(Set(s))
          if(reachables.contains(i)){
            haha.getOrElseUpdate(i, msetEmpty) += s
          }
          println("d")
        })
        println("d")
      }
    })
    print("")
  }

  def getReachableMethodsBySBCG(global: Global, typs: ISet[Signature]): Unit = {
//    val lol = typs.map(typ => {
//      global.getClazz(typ) match {
//        case Some(c) => c.getDeclaredMethods.map(_.getSignature)
//        case None => isetEmpty[Signature]
//      }
//    })

    val cg = SignatureBasedCallGraph(global, typs)
    println("")
  }


  def parse(apk: ApkGlobal, yard: ApkYard): Unit = {
    val handler: AndroidModelCallHandler = new AndroidModelCallHandler
    val sm: SummaryManager = new AndroidSummaryProvider(apk).getSummaryManager
    val analysis = new BottomUpSummaryGenerator[Global](apk, sm, handler, PTSummary(_, _), ConsoleProgressBar.on(System.out).withFormat("[:bar] :percent% :elapsed Left: :remain"))
    val signatures: ISet[Signature] = apk.model.getComponentInfos.flatMap(apk.getEntryPoints)
    val callGraph = SignatureBasedCallGraph(apk, signatures, None)

    val orderedWUs: IList[WorkUnit[Global]] = callGraph.topologicalSort(true).map { sig =>
      val method = apk.getMethodOrResolve(sig).getOrElse(throw new RuntimeException("Method does not exist: " + sig))
      new IntentWu(apk, method, sm, handler, store, "intent")
    }

    analysis.build(orderedWUs)
  }

  def collectIntents(apk: ApkGlobal): Unit = {
    val candidate = store.getPropertyOrElse[MSet[(Context, PTASlot)]]("intent", msetEmpty)

    val explicitIntents: MSet[(Context, Intent)] = msetEmpty

    candidate.foreach { case (ctx, s) =>
      val intentInss = store.resolved.pointsToSet(ctx, s)
      val intent = IntentHelper.getIntentContents(store.resolved, intentInss, ctx)

      intent.foreach(u => {
        println("Found intent: " + intent)
        explicitIntents.add(ctx, u)
      })
    }

    println()

    explicitIntents.foreach {
      case (context, intent) =>
        if (intent.componentNames.nonEmpty) {
          intent.componentNames.foreach(name => {
            println(context.getMethodSig.classTyp + " -ICC-> " + name + " by method " + context.getMethodSig.methodName)
            graph = graph :+ ((context.getMethodSig.classTyp.jawaName, name, context.getMethodSig.methodName))
            iets.add(context.getMethodSig)
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
                      if (u.equals(f)) {
                        println(context.getMethodSig.classTyp + " -ICC-Action-> " + f + " --> " + classType + " by method " + context.getMethodSig.methodName)
                        graph = graph :+ ((context.getMethodSig.classTyp.jawaName, classType.jawaName, context.getMethodSig.methodName + ";" + f))
                        iets.add(context.getMethodSig)
                      }
                    })
                  }
                })
            }
          })
        }
    }
  }
}
