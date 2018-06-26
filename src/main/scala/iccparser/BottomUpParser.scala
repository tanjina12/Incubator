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
import org.argus.amandroid.core.{AndroidConstants, AndroidGlobalConfig, ApkGlobal}
import org.argus.amandroid.summary.wu.IntentWu
import org.argus.jawa.alir.Context
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.alir.pta.PTASlot
import org.argus.jawa.alir.reachability.SignatureBasedCallGraph
import org.argus.jawa.alir.util.ExplicitValueFinder
import org.argus.jawa.ast.{AssignmentStatement, CallStatement, Location}
import org.argus.jawa.core._
import org.argus.jawa.core.util.{MSet, _}
import org.argus.jawa.summary.taint.BottomUpTaintAnalysis
import org.argus.jawa.summary.wu._
import org.argus.jawa.summary.{BottomUpSummaryGenerator, SummaryManager}
import writer.MethodWriter


class TestSinkManager(sasFilePath: String) extends AndroidSourceAndSinkManager(sasFilePath) {

  private final val TITLE = "DefaultSourceAndSinkManager"

  override def isUISource(apk: ApkGlobal, calleeSig: Signature, callerSig: Signature, callerLoc: Location): Boolean = {
    if (calleeSig.signature == AndroidConstants.ACTIVITY_FINDVIEWBYID || calleeSig.signature == AndroidConstants.VIEW_FINDVIEWBYID) {
      val ok = apk.getMethodOrResolve(callerSig).get.getBody.resolvedBody.locations
      val lol = calleeSig.getParameters
      val lol1 = calleeSig.getParameterTypes
      val lol2 = callerLoc.statement.asInstanceOf[CallStatement]
      val lol3 = lol2.lhsOpt.get.varSymbol

      print()
//      val callerProc = apk.getMethod(callerSig).get
//      val cs = callerLoc.statement.asInstanceOf[CallStatement]
//      val nums = ExplicitValueFinder.findExplicitLiteralForArgs(callerProc, callerLoc, cs.arg(0))
//      nums.filter(_.isInt).foreach { num =>
//        apk.model.getLayoutControls.get(num.getInt) match {
//          case Some(control) =>
//            return control.isSensitive
//          case None =>
//            apk.reporter.echo(TITLE, "Layout control with ID " + num + " not found.")
//        }
//      }
    }
    false
  }
}


class WidgetAndCallBackManager(reporter: Reporter) {

  val callbackClasses: MMap[JawaClass, Location] = mmapEmpty
  val callBackMethods: MMap[Signature, MSet[Location]] = mmapEmpty
  val androidCallBacks = new AndroidCallBacks().initAndroidCallbacks

  def isUISource(apk: ApkGlobal, calleeSig: Signature, callerSig: Signature, callerLoc: Location): Unit = {


    val param = calleeSig.getParameterTypes.head
    val typRecOpt = apk.getClazz(param)
    val callbackClasses: MSet[JawaClass] = msetEmpty

    typRecOpt match {
      case Some(typRec) =>
        val hier = apk.getClassHierarchy
        if (typRec.isInterface) {
          val parent = apk.getClassOrResolve(callerSig.classTyp)
          if (parent.getInterfaces.contains(typRec)) {
            this.callbackClasses.getOrElseUpdate(parent, callerLoc)
          }
        }
      case None =>
    }

    this.callbackClasses.foreach(x => {
      reporter.println("qAnalyse class " + x._1 + " in lifecyce " + callerSig.classTyp)
      val locs: MSet[Location] = msetEmpty
      locs.add(x._2)
      analyzeClass(x._1, callerSig.classTyp, locs)
    })
  }

  def lolol(apk: ApkGlobal, cbb: MMap[(Signature, JawaType), MSet[Location]]): Unit = {


    cbb.foreach(x => {
      reporter.println("Analyse method " + x._1._1 + " calls class " + x._1._2)
      val lifeCycle = x._1._1.classTyp
      val listenerInit = apk.getClassOrResolve(x._1._2)
      analyzeClass(listenerInit, lifeCycle, x._2)
    })

  }

  private def analyzeClass(clazz: JawaClass, lifecycleElement: JawaType, location: MSet[Location]): Unit = {
    // Do not analyze system classes
    if (clazz.getName.startsWith("android.") || clazz.getName.startsWith("com.android.") || clazz.getName.startsWith("java."))
      return

    // Check for callback handlers implemented via interfaces
    analyzeClassInterfaceCallbacks(clazz, clazz, lifecycleElement, location)
  }

  private def analyzeClassInterfaceCallbacks(baseClass: JawaClass, clazz: JawaClass, lifecycleElement: JawaType, location: MSet[Location]): Unit = {
    // We cannot create instances of abstract classes anyway, so there is no
    // reason to look for interface implementations
    if (!baseClass.isConcrete) {
      return
    }

    // For a first take, we consider all classes in the android.* packages
    // to be part of the operating system
    if (baseClass.getName.startsWith("android.") || baseClass.getName.startsWith("com.android.")) {
      return
    }

    // If we are a class, one of our superclasses might implement an Android
    // interface
    if (clazz.hasSuperClass) {
      analyzeClassInterfaceCallbacks(baseClass, clazz.getSuperClass, lifecycleElement, location) // recursion
    }
    // Do we implement one of the well-known interfaces?
    for (i <- collectAllInterfaces(clazz)) {
      if (this.androidCallBacks.contains(i.getName)) {
        i.getDeclaredMethods.foreach { proc =>
          getMethodFromHierarchy(baseClass, proc.getSubSignature).foreach { m =>
            checkAndAddMethod(m, lifecycleElement, location)
          }
        }
      }
    }
  }

  private def checkAndAddMethod(proc: JawaMethod, lifecycleElement: JawaType, location: MSet[Location]) = {
    if (!proc.getFullName.startsWith("android.")) {
      this.callBackMethods.getOrElseUpdate(proc.getSignature, msetEmpty) ++= location
    }
  }

  private def collectAllInterfaces(ar: JawaClass): ISet[JawaClass] = {
    if (ar.getInterfaceSize == 0) isetEmpty
    else ar.getInterfaces ++ ar.getInterfaces.flatMap { i => collectAllInterfaces(i) }
  }

  private def getMethodFromHierarchy(r: JawaClass, subSig: String): Option[JawaMethod] = {
    try {
      if (r.declaresMethod(subSig)) r.getMethod(subSig)
      else if (r.hasSuperClass) getMethodFromHierarchy(r.getSuperClass, subSig)
      else None
    } catch {
      case _: Exception =>
        None // Add this to tentatively avoid issue #22
    }
  }
}

class BottomUpParser(store: PTStore) extends BaseAppParser {

  var iccMethods: MSet[Signature] = msetEmpty
  val ssm = new WidgetAndCallBackManager(new PrintReporter(MsgLevel.INFO))
  var callGraph: CallGraph = new CallGraph()

  def writeMethods(writer: MethodWriter, apk: ApkGlobal) = {
    val components = collectComponents(apk)
    val methods = collectMethods(apk, components)
    writer.write(methods)
  }

  def parse2(apk: ApkGlobal, yard: ApkYard, callGraph: CallGraph): Unit = {
    val reporter = new PrintReporter(MsgLevel.INFO)
    val ssm = new TestSinkManager(AndroidGlobalConfig.settings.sas_file)
    val ta = new BottomUpTaintAnalysis[ApkGlobal](apk, new AndroidSummaryProvider(apk), new AndroidModelCallHandler, ssm, reporter)
    val eps = apk.model.getEnvMap.map(_._2._1).toSet
    ta.process(eps)
  }

  def tempTest(apk: ApkGlobal, reporter: Reporter): Unit = {
    val cg = SignatureBasedCallGraph(apk, apk.getApplicationClasses.flatMap(x => x.getDeclaredMethods).map(x => x.getSignature))
    val androidCallBacks = new AndroidCallBacks().initAndroidCallbacks


    val ClassInitByLoc: MMap[(Signature, JawaType), MSet[Location]] = mmapEmpty


    cg.getCallMap.foreach(x => {
      x._2.foreach(j => {
        if (j.getParameterTypes.exists(i => androidCallBacks.contains(i.baseTyp))) {
          reporter.println("This method has listener : " + x._1 + " calls " + j)

          val methodBody = apk.getMethodOrResolve(x._1).get.getBody.resolvedBody

          val interfaceLocations = methodBody.locations.filter(_.statement match {
            case cs: CallStatement => cs.signature.methodName == j.methodName
            case _ => false
          })

          interfaceLocations.foreach(i => {
            val initIndex = i.locationIndex - 1
            val locationInitMethod = methodBody.location(initIndex)
            locationInitMethod.statement match {
              case cs: CallStatement =>
                if (cs.signature.methodName == "<init>") {
                  ClassInitByLoc.getOrElseUpdate((x._1, cs.signature.getClassType), msetEmpty) += i
                  reporter.println("Method " + j + " inits " + cs.signature.getClassName)
                } else {
                  ssm.isUISource(apk, j, x._1, i)
                }
              case _ => ssm.isUISource(apk, j, x._1, i)
            }
          })
        }
      })
    })


    ssm.lolol(apk, ClassInitByLoc)
    println("")
  }

  def collectTypes(apk: ApkGlobal, reporter: Reporter): Unit = {
    println("Finding widgets...")
    val indirectCallGraph: MMap[Signature, MSet[Signature]] = mmapEmpty
    val callbackMethodAndWidgetId = AppInfoCollector.layoutIdWithCallBackMethods
    val lol = tempTest(apk, reporter)
    val callbackMethodAndWidgetLoc = ssm.callBackMethods
    val callbackMethodAndWidget: MMap[String, MSet[Signature]] = mmapEmpty


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
      x._2.foreach(j => {
        val widget = j.statement match {
          case cs: CallStatement =>
            cs.signature.classTyp.jawaName
          case _ =>
            reporter.error("CollectWidgets", "Unknown error during widget collection for loc")
            "Unknown"
        }
        val methods = x._1
        callbackMethodAndWidget.getOrElseUpdate(widget, msetEmpty) += methods
      })
    })


    iccMethods.foreach(m => {
      val allWidgets = callbackMethodAndWidget.values.flatten.toSet
      if (allWidgets.contains(m)) {
        val widgets = callbackMethodAndWidget.filter(x => x._2.contains(m))
        widgets.foreach(widget => {
          reporter.println("-id- Method " + m + " called by " + widget._1)
        })
      }
    })

    indirectCallGraph.foreach(m => {
      val allWidgets = callbackMethodAndWidget.values.flatten.toSet
      if (allWidgets.exists(m._2.contains)) {
        m._2.foreach(i => {
          val widgets = callbackMethodAndWidget.filter(j => j._2.contains(i))
          widgets.foreach(widget => {
            reporter.println("-loc- Method " + m._1 + " called by " + widget._1)
          })
        })
      }
    })
    println()
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

//    parse2(apk, yard, callGraph)
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
