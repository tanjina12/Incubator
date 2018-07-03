package iccparser

import callbacks.{AndroidCallBacks, WidgetAndCallBackManager}
import hu.ssh.progressbar.console.ConsoleProgressBar
import org.argus.amandroid.alir.componentSummary.ApkYard
import org.argus.amandroid.alir.pta.model.AndroidModelCallHandler
import org.argus.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.argus.amandroid.alir.pta.summaryBasedAnalysis.AndroidSummaryProvider
import org.argus.amandroid.core.ApkGlobal
import org.argus.amandroid.core.appInfo.AppInfoCollector
import org.argus.amandroid.core.model.Intent
import org.argus.amandroid.summary.wu.IntentWu
import org.argus.jawa.alir.Context
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.alir.pta.PTASlot
import org.argus.jawa.alir.reachability.SignatureBasedCallGraph
import org.argus.jawa.ast.{CallStatement, Location}
import org.argus.jawa.core._
import org.argus.jawa.core.util.{MSet, _}
import org.argus.jawa.summary.wu._
import org.argus.jawa.summary.{BottomUpSummaryGenerator, SummaryManager}
import polyglot.types.ClassType
import writer.MethodWriter


class BottomUpParser(store: PTStore) extends BaseAppParser {

  var iccMethods: MMap[Signature, (String, String, String)] = mmapEmpty
  val ssm = new WidgetAndCallBackManager(new PrintReporter(MsgLevel.WARNING))
  var callGraph: CallGraph = new CallGraph()

  // Source, Target, Method - Widgets

  def writeMethods(writer: MethodWriter, apk: ApkGlobal) = {
    val components = collectComponents(apk)
    val methods = collectMethods(apk, components)
    writer.write(methods)
  }


  def collectCallbackWidgetsFromLoc(apk: ApkGlobal, reporter: Reporter): Unit = {
    val androidCallBacks = new AndroidCallBacks().initAndroidCallbacks

    //Original method onCreate, inits class $1(jawatype) via Listeners(loc)
    val ClassInitByLoc: MMap[(Signature, JawaType), MSet[Location]] = mmapEmpty

    callGraph.getCallMap.foreach(x => {
      x._2.foreach(j => {
        if (j.getParameterTypes.exists(i => androidCallBacks.contains(i.baseTyp))) {

          val methodBody = apk.getMethodOrResolve(x._1).get.getBody.resolvedBody

          val interfaceLocations = methodBody.locations.filter(_.statement match {
            case cs: CallStatement => cs.signature.methodName == j.methodName
            case _ => false
          })

          interfaceLocations.foreach(i => {
            val initIndex = i.locationIndex - 1
            if (initIndex >= 0) {
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
            }
          })
        }
      })
    })


    ssm.collectWidgetsForInitListener(apk, ClassInitByLoc)
  }


  def bindWidgetsToIccMethods(apk: ApkGlobal, reporter: Reporter): Unit = {
    println("Finding widgets...")
    val indirectCallGraph: MMap[Signature, MSet[Signature]] = mmapEmpty
    val indirectCallGraph1: MMap[Signature, MSet[Signature]] = mmapEmpty
    val callbackMethodAndWidgetId = AppInfoCollector.layoutIdWithCallBackMethods

    reporter.println("Collect widgets from loc")
    collectCallbackWidgetsFromLoc(apk, reporter)
    val callbackMethodAndWidgetLoc = ssm.callBackMethods

    val callbackMethodAndWidget: MMap[Signature, MSet[JawaType]] = mmapEmpty

    callbackMethodAndWidgetId.foreach(x => {
      x._2.foreach(j => {
        callbackMethodAndWidget.getOrElseUpdate(x._1, msetEmpty) += j._2.typ
      })
    })

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

    apk.model.getCallbackMethods.diff(callbackMethodAndWidget.keySet).foreach(x => {
      if (x.methodName == "onOptionsItemSelected") {
        apk.getMethod(x).get.getParamTypes.foreach(j => {
          if (j.baseType.packageName == "android.view") {
            callbackMethodAndWidget.getOrElseUpdate(x, msetEmpty) += j
          }
        })
      } else {
        reporter.warning("Collect", "CallbackMethod " + x + " does not have a widget")
      }
    })


    iccMethods.foreach(m => {
      var callbackMethods = apk.model.getCallbackMethods
      callbackMethods ++= callbackMethodAndWidget.keySet.diff(callbackMethods)

      callbackMethods.foreach(s => {
        if (s != m._1) {
          val reachables = callGraph.getReachableMethods(Set(s))
          if (reachables.contains(m._1)) {
            indirectCallGraph.getOrElseUpdate(m._1, msetEmpty) += s
          }
        }
      })
    })

    iccMethods.foreach(m => {
      if (!callbackMethodAndWidget.contains(m._1)) {
        callbackMethodAndWidget.foreach(y => {
          val reachables = callGraph.getReachableMethods(Set(y._1))
          if (reachables.contains(m._1)) {
            indirectCallGraph1.getOrElseUpdate(m._1, msetEmpty) += y._1
          }
        })
      }
    })

    //    printICCWidget(apk, callbackMethodAndWidget, reporter)
    //    printIntentsWidget(callbackMethodAndWidget, reporter)
    //    printWidget(callbackMethodAndWidget, reporter)


    iccMethods.foreach(iccMethod => {
      if (callbackMethodAndWidget.contains(iccMethod._1)) {
        val widgets = callbackMethodAndWidget(iccMethod._1)
        widgets.foreach(widget => {
          reporter.println("-id- Method " + iccMethod._1 + " called by " + widget.jawaName)
          this.graph.getOrElseUpdate(iccMethod._2, msetEmpty) += widget.jawaName
        })
      } else if (indirectCallGraph.contains(iccMethod._1)) {
        val callbackMethods = indirectCallGraph(iccMethod._1)

        if (callbackMethods.exists(callbackMethodAndWidget.contains)) {
          callbackMethods.foreach(method => {
            if (callbackMethodAndWidget.contains(method)) {
              val widgets = callbackMethodAndWidget(method)
              widgets.foreach(widget => {
                reporter.println("-loc- Method " + iccMethod._1 + " called by " + widget.jawaName)
                this.graph.getOrElseUpdate(iccMethod._2, msetEmpty) += widget.jawaName
              })
            }
          })
        } else {
          reporter.error("BindingWidgets", "No widget found for indirectCallback " + iccMethod._1)
        }
      } else {
        reporter.error("BindingWidgets", "No widget found for iccMethod " + iccMethod._1)
      }
    })

    println()
  }

  def okeoke(apk: ApkGlobal): Unit = {

    val iccClasses = iccMethods.keySet.map(x => x.classTyp)

    val entryPoints = apk.model.getComponentInfos.flatMap(apk.getEntryPoints)
    entryPoints.foreach(entryPoint => {
      //all methods
      val m = apk.getMethod(entryPoint).get


      val reachableMethods = callGraph.getReachableMethods(Set(m.getSignature))
      val reachableClasses = reachableMethods.groupBy(x => x.classTyp).filter(x => x._1 != m.getDeclaringClass.getType)

      //reachable contains one of the icc methods
      if (reachableClasses.keySet.exists(iccClasses.contains)) {
        val lp1 = reachableClasses.filter(x => iccClasses.contains(x._1))
        //Found alternative way of reaching icc class
        lp1.foreach(ok => {
          val iccClass = apk.getClazz(ok._1).get
          if(!iccClass.isInnerClass){
            println("class" + m.getDeclaringClass.getType.jawaName + "--direct INIT-->" + ok._1.jawaName + " method " + m.getSignature)
          }
        })

      } else {
        iccClasses.foreach(j => {
          val iccClass = apk.getClazz(j).get

          // more detailed
          //            reachableClasses.foreach(x => {
          //              x._2.foreach(s => {
          //
          //                val mm = apk.getMethod(s).get
          //                if(mm.isConstructor && mm.getDeclaringClass.isApplicationClass){
          //                  var lifeCycleMethods = mm.getDeclaringClass.getDeclaredMethods.filter(x => x.isOverride && x.isConstructor).groupBy(x => x.getDeclaringClass.getType)
          //                  if(lifeCycleMethods.keySet.exists(iccClasses.contains)){
          //                    println()
          //                  }
          //                }
          //              })
          //            })


          if (iccClass.isInnerClass) {
            if (reachableClasses.keySet.contains(iccClass.getOuterType.get)) {
              val lp = reachableClasses.filter(x => x._1 == iccClass.getOuterType.get)
              // the parent class is init by

              lp.foreach(ok => {
                println("class" + m.getDeclaringClass.getType.jawaName + "--INIT-->" + ok._1.jawaName + " method " + m.getSignature)
              })
            }
          }
        })
      }
    })
    println()
  }

  def recursiveLifeCycleMethods(constructor: JawaMethod, cls: ClassType): Unit = {

  }

  def addWidget(graph: (String, String, String), widget: String): Unit = {
    this.graph.getOrElseUpdate(graph, msetEmpty) += widget
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
            graph.getOrElseUpdate((context.getMethodSig.classTyp.jawaName, name, context.getMethodSig.methodName), msetEmpty)
            iccMethods.getOrElseUpdate(context.getMethodSig, (context.getMethodSig.classTyp.jawaName, name, context.getMethodSig.methodName))
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
                        graph.getOrElseUpdate((context.getMethodSig.classTyp.jawaName, classType.jawaName, context.getMethodSig.methodName + ";" + f), msetEmpty)
                        iccMethods.getOrElseUpdate(context.getMethodSig, (context.getMethodSig.classTyp.jawaName, classType.jawaName, context.getMethodSig.methodName + ";" + f))
                      }
                    })
                  }
                })
            }
          })
        }
    }
    okeoke(apk)
  }

  def printWidget(callbackMethodAndWidget: MMap[Signature, MSet[JawaType]], reporter: Reporter): Unit = {
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
    reporter.println("--------- ICC METHODS ")
    iccMethods.foreach(iccMethod => {

      //DIRECT
      if (callbackMethodAndWidget.contains(iccMethod._1)) {
        val widgets = callbackMethodAndWidget(iccMethod._1)
        widgets.foreach(w => {
          reporter.println("DIRECT Method: " + iccMethod._1)
          reporter.println("by widget " + w.jawaName)
        })
        //INDIRECT
      } else {
        apk.model.getCallbackMethods.foreach(cb => {
          val reachables = callGraph.getReachableMethods(Set(cb))
          // INDIRECT
          if (reachables.contains(iccMethod._1)) {
            val widgets = callbackMethodAndWidget(cb)
            widgets.foreach(w => {
              reporter.println("INDIRECT Method: " + iccMethod._1)
              reporter.println("by widget " + w.jawaName)
            })
          }
        })
      }
    })
    reporter.println("--------- -----------")
  }

  def printIntentsWidget(callbackMethodAndWidget: MMap[Signature, MSet[JawaType]], reporter: Reporter): Unit = {
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
}
