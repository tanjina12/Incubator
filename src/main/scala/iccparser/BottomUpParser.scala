package iccparser

import callbacks.AndroidCallBacks
import hu.ssh.progressbar.console.ConsoleProgressBar
import org.argus.amandroid.alir.componentSummary.ApkYard
import org.argus.amandroid.alir.pta.model.AndroidModelCallHandler
import org.argus.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.argus.amandroid.alir.pta.summaryBasedAnalysis.AndroidSummaryProvider
import org.argus.amandroid.alir.taintAnalysis.AndroidSourceAndSinkManager
import org.argus.amandroid.core.appInfo.AppInfoCollector
import org.argus.amandroid.core.model.Intent
import org.argus.amandroid.core.{AndroidGlobalConfig, ApkGlobal}
import org.argus.amandroid.summary.wu.IntentWu
import org.argus.jawa.alir.Context
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.alir.pta.PTASlot
import org.argus.jawa.alir.reachability.SignatureBasedCallGraph
import org.argus.jawa.ast.{AssignmentStatement, CallStatement, Location}
import org.argus.jawa.core._
import org.argus.jawa.core.util._
import org.argus.jawa.summary.taint.BottomUpTaintAnalysis
import org.argus.jawa.summary.wu._
import org.argus.jawa.summary.{BottomUpSummaryGenerator, SummaryManager}
import writer.MethodWriter

class WidgetAndCallBackManager(sasFilePath: String) extends AndroidSourceAndSinkManager(sasFilePath) {
  private final val TITLE = "WidgetAndCallBackManager"
  val callbackClasses: MMap[JawaClass, Location] = mmapEmpty
  val androidCallBacks = new AndroidCallBacks().initAndroidCallbacks

  override def isUISource(apk: ApkGlobal, calleeSig: Signature, callerSig: Signature, callerLoc: Location): Boolean = {
    val params = calleeSig.getParameterTypes.map(x => x.name)
    if (params.exists(androidCallBacks.contains)) {

      calleeSig.getParameterTypes.foreach(param => {
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

        //analyzeClass -< moet hier komnen

        this.callbackClasses ++= callbackClasses.map(x => x -> callerLoc)
        return true
      })
    }
    false
  }
}

class BottomUpParser(store: PTStore) extends BaseAppParser {

  var iccMethods: MSet[Signature] = msetEmpty
  val ssm = new WidgetAndCallBackManager(AndroidGlobalConfig.settings.sas_file)
  var callGraph: CallGraph = new CallGraph()

  def writeMethods(writer: MethodWriter, apk: ApkGlobal) = {
    val components = collectComponents(apk)
    val methods = collectMethods(apk, components)
    writer.write(methods)
  }

  def parse2(apk: ApkGlobal, yard: ApkYard, callGraph: CallGraph): Unit = {
    val reporter = new PrintReporter(MsgLevel.INFO)
    val ta = new BottomUpTaintAnalysis[ApkGlobal](apk, new AndroidSummaryProvider(apk), new AndroidModelCallHandler, ssm, reporter)
    val eps = apk.model.getEnvMap.map(_._2._1).toSet
    ta.process(eps)
  }

  def collectTypes(apk: ApkGlobal, reporter: Reporter): Unit = {
    println("Finding widgets...")
    val indirectCallGraph: MMap[Signature, MSet[Signature]] = mmapEmpty
    val callbackMethodAndWidgetId = AppInfoCollector.layoutIdWithCallBackMethods
    val callbackMethodAndWidgetLoc = ssm.callbackClasses
    val callbackMethodAndWidget: MMap[String, MSet[Signature]] = mmapEmpty

    callbackMethodAndWidgetId.foreach(x => {
      val widget = apk.model.getLayoutControls.get(x._1) match {
        case Some(l) => l.viewClass.jawaName
        case None =>
          x._2.foreach(cb => {
            reporter.error("CollectWidgets", "Could not find suitable widget for ID: " + x._1 + " and callbackmethod " + cb.methodName)
          })
          "Unknown"
      }
      callbackMethodAndWidget.getOrElseUpdate(widget, msetEmpty) ++= x._2
    })

    callbackMethodAndWidgetLoc.foreach(x => {
      val widget = x._2.statement match {
        case cs: CallStatement =>
          cs.signature.classTyp.jawaName
        case as: AssignmentStatement =>
          println("sadfasdfsafsaf")
          "Unknown"
      }
      val methods = x._1.getDeclaredMethods.map(x => x.getSignature)
      callbackMethodAndWidget.getOrElseUpdate(widget, msetEmpty) ++= methods
    })
    println()


    iccMethods.foreach(m => {
      if (!apk.model.getCallbackMethods.contains(m)) {
        apk.model.getCallbackMethods.foreach(s => {
          val reachables = callGraph.getReachableMethods(Set(s))
          if (reachables.contains(m)) {
            indirectCallGraph.getOrElseUpdate(m, msetEmpty) += s
          }
        })
      }
    })

    iccMethods.foreach(m => {
      val allWidgets = callbackMethodAndWidget.values.flatten.toSet
      if (allWidgets.contains(m)) {
        val widgets = callbackMethodAndWidget.filter(x => x._2.contains(m))
        widgets.foreach(widget => {
          println("-1- Method " + m + " called by " + widget._1)
        })
      }
      //      else {
      //        haha.get(m) match {
      //          case Some(methods) =>
      //            methods.foreach(i => {
      //              val widgets = callbackMethodAndWidget.filter(j => j._2.contains(i))
      //              widgets.foreach(widget => {
      //                println("-2- Method " + m + " called by " + widget._1)
      //              })
      //            })
      //          case None => println("huh")
      //        }
      //      }
    })

    indirectCallGraph.foreach(m => {
      val allWidgets = callbackMethodAndWidget.values.flatten.toSet
      if (allWidgets.exists(m._2.contains)) {
        m._2.foreach(i => {
          val widgets = callbackMethodAndWidget.filter(j => j._2.contains(i))
          widgets.foreach(widget => {
            println("-2- Method " + m._1 + " called by " + widget._1)
          })
        })
      }
    })

  }

  def parse(apk: ApkGlobal, yard: ApkYard): Unit = {
    val handler: AndroidModelCallHandler = new AndroidModelCallHandler
    val sm: SummaryManager = new AndroidSummaryProvider(apk).getSummaryManager
    val analysis = new BottomUpSummaryGenerator[Global](apk, sm, handler, PTSummary(_, _), ConsoleProgressBar.on(System.out).withFormat("[:bar] :percent% :elapsed Left: :remain"))
    val signatures: ISet[Signature] = apk.model.getComponentInfos.flatMap(apk.getEntryPoints)
    callGraph = SignatureBasedCallGraph(apk, signatures, None)

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
            iccMethods.add(context.getMethodSig)
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
                        iccMethods.add(context.getMethodSig)
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
