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


class WidgetAndCallBackManager(reporter: Reporter) {

  val callbackClasses: MMap[JawaClass, Location] = mmapEmpty
  val callBackMethods: MMap[Signature, MSet[Location]] = mmapEmpty
  val androidCallBacks = new AndroidCallBacks().initAndroidCallbacks

  def collectWidgetsFromParentClass(apk: ApkGlobal, calleeSig: Signature, callerSig: Signature, callerLoc: Location): Unit = {


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
      val locs: MSet[Location] = msetEmpty
      locs.add(x._2)
      analyzeClass(x._1, callerSig.classTyp, locs)
    })
  }

  def collectWidgetsForInitListener(apk: ApkGlobal, cbb: MMap[(Signature, JawaType), MSet[Location]]): Unit = {
    cbb.foreach(x => {
      val lifeCycle = x._1._1.classTyp
      val listenerInit = apk.getClassOrResolve(x._1._2)
      analyzeClass(listenerInit, lifeCycle, x._2)
    })

  }

  private def analyzeClass(clazz: JawaClass, lifecycleElement: JawaType, location: MSet[Location]): Unit = {
    // Do not analyze system classes
    if (clazz.getName.startsWith("android.") || clazz.getName.startsWith("com.android.") || clazz.getName.startsWith("java."))
      return

    //    reporter.println("")
    //    reporter.println("Collect internal methods for " + clazz)

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
      //      reporter.println("Found internal method " + proc.getSignature)
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


  def collectCallbackWidgetsFromLoc(apk: ApkGlobal, reporter: Reporter): Unit = {
    val cg = callGraph
    val androidCallBacks = new AndroidCallBacks().initAndroidCallbacks

    //Original method onCreate, inits class $1(jawatype) via Listeners(loc)
    val ClassInitByLoc: MMap[(Signature, JawaType), MSet[Location]] = mmapEmpty


    cg.getCallMap.foreach(x => {
      x._2.foreach(j => {
        if (j.getParameterTypes.exists(i => androidCallBacks.contains(i.baseTyp))) {

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
                  //                  reporter.println("Method " + x._1 + " listener " + j.methodName + " inits " + cs.signature.getClassName)
                } else {
                  ssm.collectWidgetsFromParentClass(apk, j, x._1, i)
                }
              case _ => ssm.collectWidgetsFromParentClass(apk, j, x._1, i)
            }
          })
        }
      })
    })


    ssm.collectWidgetsForInitListener(apk, ClassInitByLoc)
    println("")
  }

  def printWidget(callbackMethodAndWidget: MMap[Signature, MSet[JawaType]], reporter: Reporter): Unit = {
    // GET ALL METHODS THAT CAN REACH AN ICC METHOD
    // IF METHOD IS AN CALLBACK THEN WE CAN ADD IT AS WIDGET

    // SO DIRECT METHODS
    // AND INDIRECT METHODS
    reporter.println("")
    callbackMethodAndWidget.foreach(x => {
      x._2.foreach(j => {
        reporter.println("Method: " + x)
        reporter.println("by widget " + j.jawaName)
      })
    })
    reporter.println("")
  }

  def printICCWidget(apk: ApkGlobal, callbackMethodAndWidget: MMap[Signature, MSet[JawaType]], reporter: Reporter): Unit = {
    // GET ALL METHODS THAT CAN REACH AN ICC METHOD
    // IF METHOD IS AN CALLBACK THEN WE CAN ADD IT AS WIDGET

    // SO DIRECT METHODS
    // AND INDIRECT METHODS

    reporter.println("--------- ICC METHODS ")
    iccMethods.foreach(iccMethod => {

      //DIRECT
      if (callbackMethodAndWidget.contains(iccMethod)) {
        val widgets = callbackMethodAndWidget(iccMethod)
        widgets.foreach(w => {
          reporter.println("DIRECT Method: " + iccMethod)
          reporter.println("by widget " + w.jawaName)
        })
        //INDIRECT
      } else {
        apk.model.getCallbackMethods.foreach(cb => {
          val reachables = callGraph.getReachableMethods(Set(cb))
          // INDIRECT
          if (reachables.contains(iccMethod)) {
            val widgets = callbackMethodAndWidget(cb)
            widgets.foreach(w => {
              reporter.println("INDIRECT Method: " + iccMethod)
              reporter.println("by widget " + w.jawaName)
            })
          }
        })
      }
    })
    reporter.println("--------- -----------")
  }

  def printIntentsWidget(callbackMethodAndWidget: MMap[Signature, MSet[JawaType]], reporter: Reporter): Unit = {
    // GET ALL METHODS THAT CAN REACH AN ICC METHOD
    // IF METHOD IS AN CALLBACK THEN WE CAN ADD IT AS WIDGET

    // SO DIRECT METHODS
    // AND INDIRECT METHODS
    reporter.println("++++++++++ Finding intents")
    callbackMethodAndWidget.foreach(x => {
      val reachables = callGraph.getReachableMethods(Set(x._1))

      if (reachables.exists(x => x.methodName == "startActivity"
        || x.methodName == "startActivityForResult"
        || x.methodName == "startActivityFromFragment"
        || x.methodName == "startActivityIfNeeded"
        || x.methodName == "startActivityIfNeeded"
        || x.methodName == "startActivityFromChild")) {
        x._2.foreach(k => {
          println("Method can reach intent ")
          println("Method: " + x._1)
          println("Widget: " + k.jawaName)
          println()
        })
      }

    })
    reporter.println("++++++++++ Finished")
  }

  def collectTypes(apk: ApkGlobal, reporter: Reporter): Unit = {
    println("Finding widgets...")
    val indirectCallGraph: MMap[Signature, MSet[Signature]] = mmapEmpty
    val indirectCallGraph1: MMap[Signature, MSet[Signature]] = mmapEmpty
    val callbackMethodAndWidgetId = AppInfoCollector.layoutIdWithCallBackMethods

    reporter.println("Collect widgets from loc")
    collectCallbackWidgetsFromLoc(apk, reporter)
    val callbackMethodAndWidgetLoc = ssm.callBackMethods

    val callbackMethodAndWidget: MMap[Signature, MSet[JawaType]] = mmapEmpty


    ///TEST
    callbackMethodAndWidgetId.foreach(x => {
      x._2.foreach(j => {
        callbackMethodAndWidget.getOrElseUpdate(x._1, msetEmpty) += j._2.typ
      })
    })

    //TEST
    callbackMethodAndWidgetLoc.foreach(x => {
      x._2.foreach(j => {
        j.statement match {
          case cs: CallStatement =>
            callbackMethodAndWidget.getOrElseUpdate(x._1, msetEmpty) += cs.signature.classTyp
          case _ =>
            reporter.error("CollectWidgets", "Unknown error during widget collection for loc")
            "Unknown"
        }
      })
    })


    iccMethods.foreach(m => {
      var callbackMethods = apk.model.getCallbackMethods
      callbackMethods ++= callbackMethodAndWidget.keySet.diff(callbackMethods)

      callbackMethods.foreach(s => {
        if (s != m) {
          val reachables = callGraph.getReachableMethods(Set(s))
          if (reachables.contains(m)) {
            indirectCallGraph.getOrElseUpdate(m, msetEmpty) += s
          }
        }
      })
    })

    val l = callGraph.getCallMap.values.flatten.toSet
    iccMethods.foreach(m => {

      if (!callbackMethodAndWidget.contains(m)) {
        callbackMethodAndWidget.foreach(y => {
          val reachables = callGraph.getReachableMethods(Set(y._1))
          if (reachables.contains(m)) {
            indirectCallGraph1.getOrElseUpdate(m, msetEmpty) += y._1
          }
        })
      }
    })

    //    printICCWidget(apk, callbackMethodAndWidget, reporter)
    //    printIntentsWidget(callbackMethodAndWidget, reporter)
    //    printWidget(callbackMethodAndWidget, reporter)


    iccMethods.foreach(iccMethod => {
      if (callbackMethodAndWidget.contains(iccMethod)) {
        val widgets = callbackMethodAndWidget(iccMethod)
        widgets.foreach(widget =>
          reporter.println("-id- Method " + iccMethod + " called by " + widget.jawaName)
        )
      } else if (indirectCallGraph.contains(iccMethod)) {
        val callbackMethods = indirectCallGraph(iccMethod)

        if (callbackMethods.exists(callbackMethodAndWidget.contains)) {
          callbackMethods.foreach(method => {
            if (callbackMethodAndWidget.contains(method)) {
              val widgets = callbackMethodAndWidget(method)
              widgets.foreach(widget => {
                reporter.println("-loc- Method " + iccMethod + " called by " + widget.jawaName)
              })
            }
          })
        } else {
          reporter.error("BindingWidgets", "No widget found for indirectCallback " + iccMethod)
        }
      } else {
        reporter.error("BindingWidgets", "No widget found for iccMethod " + iccMethod)
      }
    })
    val lol10 = apk.model.getCallbackMethods
    val lol11 = callbackMethodAndWidget.keySet
    val lol12 = lol10.diff(lol11)
    val lol13 = lol11.diff(lol10)
    //    lol13.foreach(x =>{
    //      val reachables = callGraph.getReachableMethods(Set(x))
    //      if (reachables.exists(iccMethods.contains)){
    //        println("lololol")
    //        val poep = reachables.filter(iccMethods.contains)
    //        poep.foreach(p => {
    //          callbackMethodAndWidget(x).foreach(w =>{
    //            println("Method " + p)
    //            println("Widget " + w.jawaName)
    //          })
    //        })
    //      }
    //    })
    println()

  }

  def parse(apk: ApkGlobal, yard: ApkYard): Unit = {
    val handler: AndroidModelCallHandler = new AndroidModelCallHandler
    val sm: SummaryManager = new AndroidSummaryProvider(apk).getSummaryManager
    val analysis = new BottomUpSummaryGenerator[Global](apk, sm, handler, PTSummary(_, _), ConsoleProgressBar.on(System.out).withFormat("[:bar] :percent% :elapsed Left: :remain"))
    val signatures: ISet[Signature] = apk.model.getComponentInfos.flatMap(apk.getEntryPoints)
    callGraph = SignatureBasedCallGraph(apk, apk.getApplicationClasses.flatMap(x => x.getDeclaredMethods).map(x => x.getSignature), None)

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

//  def collectNonICCLinks(apk: ApkGlobal, callbackMethodAndWidget: MMap[Signature, MSet[JawaType]]): Unit = {
//    var callbackMethods = apk.model.getCallbackMethods
//    callbackMethods ++= callbackMethodAndWidget.keySet.diff(callbackMethods)
//
//    callbackMethods.foreach(method => {
//      val reachables = callGraph.getReachableMethods(Set(method))
//      val lol = method.classTyp
//      val lol1 = reachables.map(x => x.classTyp)
//      if (reachables.exists(a => a.classTyp != method.classTyp)) {
//        val otherClassMethods = reachables.filter(a => a.classTyp != method.classTyp)
//        if (otherClassMethods.exists(iccMethods.contains)) {
//          val icc = otherClassMethods.intersect(iccMethods)
//          icc.foreach(i => {
//            println("Jackpot " + method+ " calls " + i)
//          })
//        }
//      }
//    })
//    println()
//  }
}
