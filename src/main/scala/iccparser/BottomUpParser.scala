package iccparser

import callbacks.AndroidCallBacks
import hu.ssh.progressbar.console.ConsoleProgressBar
import org.argus.amandroid.alir.componentSummary.ApkYard
import org.argus.amandroid.alir.pta.model.AndroidModelCallHandler
import org.argus.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.argus.amandroid.alir.pta.summaryBasedAnalysis.AndroidSummaryProvider
import org.argus.amandroid.alir.taintAnalysis.AndroidSourceAndSinkManager
import org.argus.amandroid.core.appInfo.AppInfoCollector
import org.argus.amandroid.core.{AndroidConstants, AndroidGlobalConfig, ApkGlobal}
import org.argus.amandroid.core.model.Intent
import org.argus.amandroid.summary.wu.IntentWu
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.alir.{Context, JawaAlirInfoProvider}
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

class LolManager(sasFilePath: String) extends AndroidSourceAndSinkManager(sasFilePath) {
  private final val TITLE = "PasswordSourceAndSinkManager"
  val callbackClasses: MMap[JawaClass, Location] = mmapEmpty
  val androidCallBacks = new AndroidCallBacks().initAndroidCallbacks

  override def isUISource(apk: ApkGlobal, calleeSig: Signature, callerSig: Signature, callerLoc: Location): Boolean = {
    val paramss = calleeSig.getParameterTypes.map(x => x.name)
    if (paramss.exists(androidCallBacks.contains)) {

      val param = calleeSig.getParameterTypes.head

      val typRecOpt = apk.getClazz(param)
      val callbackClasses: MSet[JawaClass] = msetEmpty

      typRecOpt match {
        case Some(typRec) =>
          val hier = apk.getClassHierarchy
          if (typRec.isInterface) {
            val impls = hier.getAllImplementersOf(typRec)
            if (impls.nonEmpty) {
              callbackClasses ++= impls.map { impl =>
                hier.getAllSubClassesOfIncluding(impl)
              }.reduce(iunion[JawaClass])
            }
          } else {
            callbackClasses ++= hier.getAllSubClassesOfIncluding(typRec)
          }
        case None =>
      }

      this.callbackClasses ++= callbackClasses.map(x => x -> callerLoc)
      return true
    }
    return false
  }
}

class BottomUpParser(store: PTStore) extends BaseAppParser {

  var iets: MSet[Signature] = msetEmpty

  def writeMethods(writer: MethodWriter, apk: ApkGlobal) = {
    val components = collectComponents(apk)
    val methods = collectMethods(apk, components)
    writer.write(methods)
  }

  def parse2(apk: ApkGlobal, yard: ApkYard, callGraph: CallGraph): Unit = {
    val ssm = new LolManager(AndroidGlobalConfig.settings.sas_file)
    val reporter = new PrintReporter(MsgLevel.INFO)
    val ta = new BottomUpTaintAnalysis[ApkGlobal](apk, new AndroidSummaryProvider(apk), new AndroidModelCallHandler, ssm, reporter)
    val eps = apk.model.getEnvMap.map(_._2._1).toSet
    ta.process(eps)
    asdfsdf(apk, ssm)
  }

  def asdfsdf(apk: ApkGlobal, ssm: LolManager): Unit = {
    val haha: MMap[Signature, MSet[Signature]] = mmapEmpty
    val callBackMethods = apk.model.getCallbackMethods
    callBackMethods.foreach(x => {

    })

    AppInfoCollector.test

//    val cg = SignatureBasedCallGraph(apk, t)
//    apk.model.getCallbackMethods.foreach(x => {
//
//    })
//
//
//    iets.foreach(i => {
//      if (apk.model.getCallbackMethods.contains(i)) {
//        println("Found callback method in .... " + i.signature)
//      } else {
//
//        t.foreach(s => {
//          val reachables = cg.getReachableMethods(Set(s))
//          if (reachables.contains(i)) {
//            haha.getOrElseUpdate(i, msetEmpty) += s
//          }
//        })
//      }
//    })
//
//    val cbt = ssm.callbackClasses
//    haha.foreach(s => {
//      s._2.foreach(l => {
//        val l1 = apk.getMethodOrResolve(l).get
//        cbt.foreach(a => {
//          if (a._1.getMethods.contains(l1)) {
//            println("Found --------> " + s._1.methodName + " called by " + l.methodName + " with listener " + a._2)
//            println()
//          }
//        })
//      })
//    })
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

    parse2(apk, yard, callGraph)
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
