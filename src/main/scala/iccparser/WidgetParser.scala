package iccparser

import callbacks.{AndroidCallBacks, WidgetAndCallBackManager}
import org.argus.amandroid.core.ApkGlobal
import org.argus.amandroid.core.appInfo.AppInfoCollector
import org.argus.jawa.alir.JawaAlirInfoProvider
import org.argus.jawa.alir.cfg.{CFGNode, IntraProceduralControlFlowGraph}
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.alir.rda.ReachingDefinitionAnalysis.Result
import org.argus.jawa.alir.rda.{DefDesc, LLocDefDesc, Slot}
import org.argus.jawa.ast._
import org.argus.jawa.core._
import org.argus.jawa.core.util._

class WidgetParser(callGraph: CallGraph, iccMethods: MMap[Signature, MSet[(String, String, String)]], graph: MMap[(String, String, String), MSet[String]]) {

  val ssm = new WidgetAndCallBackManager(new PrintReporter(MsgLevel.WARNING))
  var callbackMethodAndWidgetId: MMap[Signature, MSet[(Int, JawaClass)]] = mmapEmpty

  def collectCallbackWidgetsFromLoc(apk: ApkGlobal, reporter: Reporter): Unit = {

    val androidCallBacks = new AndroidCallBacks().initAndroidCallbacks

    //Original method onCreate, inits class $1(jawatype) via Listeners(loc)
    val ClassInitByLoc: MMap[(Signature, JawaType), MSet[(Location, Option[Int])]] = mmapEmpty

    print("looping over all callgraph " + callGraph.getCallMap.size)

    val test = callGraph.getCallMap.filter(x => x._2.exists(i => i.getParameterTypes.exists(j => androidCallBacks.contains(j.baseTyp))))

    test.foreach(x => {
      print(".")
      x._2.foreach(j => {
        if (j.getParameterTypes.exists(i => androidCallBacks.contains(i.baseTyp))) {
          apk.getMethod(x._1) match {
            case Some(m) =>
              val cfg = JawaAlirInfoProvider.getCfg(m)
              val rda = JawaAlirInfoProvider.getRda(m, cfg)
              val methodBody = m.getBody.resolvedBody

              val interfaceLocations = methodBody.locations.filter(_.statement match {
                case cs: CallStatement => cs.signature.methodName == j.methodName
                case _ => false
              })

              interfaceLocations.foreach(il => {

                val slots = rda.entrySet(cfg.getNode(il.locationUri, il.locationIndex))

                val callStatement = il.statement.asInstanceOf[CallStatement]
                val size = callStatement.rhs.varSymbols.size
                if (size < 2) {
                  reporter.warning("Icorrect param size", "KK " + j + " II " + callStatement.signature)
                } else {
                  val callee = callStatement.rhs.varSymbols(size - 2)
                  val calls = callStatement.rhs.varSymbols(size - 1)

                  val callsClass = slots.filter(h => h._1.toString == calls.toCode)
                  val calleeClass = slots.filter(h => h._1.toString == callee.toCode)

                  val callerId = calleeFinder(calleeClass, callee.toCode, methodBody, rda, cfg, apk, msetEmpty)

                  val okeDit = (il, callerId)
                  callerId match {
                    case Some(id) => print("ID: " + id)
                    case None => print("Nope: ")
                  }

                  callsClass.foreach(e => {
                    e._2 match {
                      case loc: LLocDefDesc =>
                        val p = methodBody.location(loc.locIndex)
                        p.statement match {
                          case as: AssignmentStatement =>
                            as.getRhs match {
                              case ne: NewExpression =>
                                val lo = ne.typ
                                ClassInitByLoc.getOrElseUpdate((x._1, lo), msetEmpty) += okeDit
                                println("Found class to init " + lo)
                              case _ => println("wtf")
                            }
                          case _ => println("wtf")
                        }
                      case _ => ssm.collectWidgetsFromParentClass(apk, j, x._1, okeDit)
                    }
                  })
                }
              })
            case None =>
              reporter.error("Findinglocs", "This should not happen method not found in cache. For now ignoring: ")
              reporter.error("Findinglocs", "method: " + x._1)
          }
        }
      })
    })
    ssm.collectWidgetsForInitListener(apk, ClassInitByLoc)
  }

  def calleeFinder(calleeClass: ISet[(Slot, DefDesc)], calleeCode: String, methodBody: ResolvedBody, rda: Result, cfg: IntraProceduralControlFlowGraph[CFGNode], apk: ApkGlobal, tries: MSet[(Slot, DefDesc)]): Option[Int] = {
    var result: Option[Int] = None

    calleeClass.foreach(x => {
      x._2 match {
        case loc: LLocDefDesc =>
          val targetLoc = methodBody.location(loc.locIndex)
          val slots = rda.entrySet(cfg.getNode(loc.locUri, loc.locIndex))
          val slot = slots.filter(u => u._1.toString == calleeCode)
          if (tries.exists(slot.contains)) {
            return result
          } else {
            tries ++= slot
          }
          targetLoc.statement match {
            case as: AssignmentStatement =>
              as.rhs match {
                case le: LiteralExpression =>
                  try {
                    result = Some(le.constant.rawText.dropRight(1).toInt)
                  } catch {
                    case _: Exception => None
                  }
                case _ => result = calleeFinder(slot, calleeCode, methodBody, rda, cfg, apk, tries)
              }
            case _ =>
              result = calleeFinder(slot, calleeCode, methodBody, rda, cfg, apk, tries)
          }
        case _ => println("No loc")
      }
    })
    result
  }


  def bindWidgetsToIccMethods(apk: ApkGlobal, reporter: Reporter): MMap[(String, String, String), MSet[String]] = {
    println("Finding widgets...")
    val indirectCallGraph: MMap[Signature, MSet[Signature]] = mmapEmpty
    callbackMethodAndWidgetId = AppInfoCollector.layoutIdWithCallBackMethods

    reporter.println("Collect widgets from loc")
    collectCallbackWidgetsFromLoc(apk, reporter)
    reporter.println("collect widgets loc done")
    val callbackMethodAndWidgetLoc = ssm.callBackMethods

    val callbackMethodAndWidget: MMap[Signature, MSet[JawaType]] = mmapEmpty

    callbackMethodAndWidgetId.foreach(x => {
      x._2.foreach(j => {
        callbackMethodAndWidget.getOrElseUpdate(x._1, msetEmpty) += j._2.typ
      })
    })

    callbackMethodAndWidgetLoc.foreach(x => {
      x._2.foreach(j => {
        j._2 match {
          case Some(id) =>
            val layout = apk.model.getLayoutControls.get(id)
            layout match {
              case Some(layoutControl) =>
                callbackMethodAndWidget.getOrElseUpdate(x._1, msetEmpty) += layoutControl.viewClass
              case None => {
                j._1.statement match {
                  case cs: CallStatement =>
                    callbackMethodAndWidget.getOrElseUpdate(x._1, msetEmpty) += cs.signature.classTyp
                  case _ =>
                    reporter.error("CollectWidgets", "Unknown error during widget collection for loc")
                    "Unknown"
                }
              }
            }
          case None => {
            j._1.statement match {
              case cs: CallStatement =>
                callbackMethodAndWidget.getOrElseUpdate(x._1, msetEmpty) += cs.signature.classTyp
              case _ =>
                reporter.error("CollectWidgets", "Unknown error during widget collection for loc")
                "Unknown"
            }
          }
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

    reporter.println("Finished collecting widgets")
    var callbackMethods = apk.model.getCallbackMethods
    callbackMethods ++= callbackMethodAndWidget.keySet.diff(callbackMethods)
    val iccMethodsWithoutCallbacks = iccMethods.filter(x => !callbackMethods.contains(x._1))

    reporter.println("Find all reachable " + iccMethodsWithoutCallbacks.size)
    iccMethodsWithoutCallbacks.foreach(iccMethod => {
      print(".")

      callbackMethods.filter(s => s != iccMethod._1).foreach(s => {
        val reachables = callGraph.getReachableMethods(Set(s))
        if (reachables.contains(iccMethod._1)) {
          indirectCallGraph.getOrElseUpdate(iccMethod._1, msetEmpty) += s
        }
      })
    })

    //    printICCWidget(apk, callbackMethodAndWidget, reporter)
    //    printIntentsWidget(callbackMethodAndWidget, reporter)
    //    printWidget(callbackMethodAndWidget, reporter)

    reporter.println("Binding methods with widgets")

    iccMethods.foreach(iccMethod => {
      print(".")
      if (callbackMethodAndWidget.contains(iccMethod._1)) {
        val widgets = callbackMethodAndWidget(iccMethod._1)
        widgets.foreach(widget => {
          reporter.println("-id- Method " + iccMethod._1 + " called by " + widget.jawaName)
          if (iccMethod._1.getClassName.contains("AddRecipeActivity")) {
            println()
          }
          iccMethod._2.foreach(g => graph.getOrElseUpdate(g, msetEmpty) += widget.jawaName)
        })
      } else if (indirectCallGraph.contains(iccMethod._1)) {
        val callbackMethods = indirectCallGraph(iccMethod._1)

        if (callbackMethods.exists(callbackMethodAndWidget.contains)) {
          callbackMethods.foreach(method => {
            if (callbackMethodAndWidget.contains(method)) {
              val widgets = callbackMethodAndWidget(method)
              widgets.foreach(widget => {
                reporter.println("-loc- Method " + iccMethod._1 + " called by " + widget.jawaName)
                iccMethod._2.foreach(g => graph.getOrElseUpdate(g, msetEmpty) += widget.jawaName)
              })
            }
          })
        } else {
          reporter.error("BindingWidgets", "No widget found for indirectCallback " + iccMethod._1)
        }
      } else {
        if (iccMethod._1.getParameters.nonEmpty) {
          val params = apk.getMethod(iccMethod._1).get.params.filter(x => x._2.baseTyp.startsWith("android.view"))
          params.foreach(p => {
            reporter.println("-misc- Method " + iccMethod._1 + " called by " + p._2.jawaName)
            iccMethod._2.foreach(g => graph.getOrElseUpdate(g, msetEmpty) += p._2.jawaName)
          })
        } else {
          reporter.error("BindingWidgets", "No widget found for iccMethod " + iccMethod._1)
        }
      }
    })

    graph
  }

  def addWidget(graph: (String, String, String), widget: String): Unit = {
    this.graph.getOrElseUpdate(graph, msetEmpty) += widget
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
