package iccparser

import callbacks.WidgetAndCallBackManager
import hu.ssh.progressbar.console.ConsoleProgressBar
import org.argus.amandroid.alir.componentSummary.ApkYard
import org.argus.amandroid.alir.pta.model.AndroidModelCallHandler
import org.argus.amandroid.alir.pta.reachingFactsAnalysis.IntentHelper
import org.argus.amandroid.alir.pta.summaryBasedAnalysis.AndroidSummaryProvider
import org.argus.amandroid.core.{AmandroidSettings, AndroidGlobalConfig, ApkGlobal}
import org.argus.amandroid.core.model.Intent
import org.argus.amandroid.summary.wu.IntentWu
import org.argus.jawa.alir.Context
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.alir.pta.PTASlot
import org.argus.jawa.alir.reachability.SignatureBasedCallGraph
import org.argus.jawa.core._
import org.argus.jawa.core.util.{MSet, _}
import org.argus.jawa.summary.wu._
import org.argus.jawa.summary.{BottomUpSummaryGenerator, SummaryManager}
import writer.MethodWriter


class BottomUpParser(store: PTStore) extends BaseAppParser {

  var iccMethods: MMap[Signature, (String, String, String)] = mmapEmpty
  val ssm = new WidgetAndCallBackManager(new PrintReporter(MsgLevel.WARNING))
  var callGraph: CallGraph = new CallGraph()

  // Source, Target, Method - Widgets

  def writeMethods(writer: MethodWriter, apk: ApkGlobal): Unit = {
    val components = collectComponents(apk)
    val methods = collectMethods(apk, components)
    writer.write(methods)
  }


  def bindWidgetsToIccMethods(apk: ApkGlobal, reporter: Reporter): Unit = {
    val widgetParser = new WidgetParser(callGraph, iccMethods, graph)
    this.graph = widgetParser.bindWidgetsToIccMethods(apk, reporter)
    println()
  }


  def getReachableIccClasses(reachableClasses: Set[JawaType], iccClass: JawaType, m: JawaMethod): Unit = {
    val isMethodInnerClass = getInnerClass(m, iccClass)
    val isSameClass = m.getDeclaringClass.getType == iccClass
    if (reachableClasses.contains(iccClass) && !isMethodInnerClass && !isSameClass) {
      getInnerClass(m, iccClass)
      val reachableIccClasses = reachableClasses.filter(x => x == iccClass)
      reachableIccClasses.foreach(c => {
        println("class" + m.getDeclaringClass.getType.jawaName + "--INIT-->" + c.jawaName + " method " + m.getSignature.methodName)
        graph.getOrElseUpdate((m.getDeclaringClass.getType.jawaName, c.jawaName, m.getSignature.methodName), msetEmpty)
        iccMethods.getOrElseUpdate(m.getSignature, (m.getDeclaringClass.getType.jawaName, c.jawaName, m.getSignature.methodName))
      })
    }
  }

  def getInnerClass(method: JawaMethod, iccClass: JawaType): Boolean = {
    if (method.getDeclaringClass.isInnerClass) {
      val outerType = method.getDeclaringClass.getOuterType.get

      if (outerType.jawaName.contains("$")) {
        return outerType.jawaName.contains(iccClass.jawaName)
      } else {
        return method.getDeclaringClass.getOuterType.get == iccClass
      }
    }
    false
  }

  def parse(apk: ApkGlobal, yard: ApkYard): Unit = {
    val handler: AndroidModelCallHandler = new AndroidModelCallHandler
    val sm: SummaryManager = new AndroidSummaryProvider(apk).getSummaryManager
    val analysis = new BottomUpSummaryGenerator[Global](apk, sm, handler, PTSummary(_, _), ConsoleProgressBar.on(System.out).withFormat("[:bar] :percent% :elapsed Left: :remain"))
    val signatures: ISet[Signature] = apk.model.getComponentInfos.flatMap(apk.getEntryPoints)
    callGraph = SignatureBasedCallGraph(apk, apk.getApplicationClasses.flatMap(x => x.getDeclaredMethods).map(x => x.getSignature), None)
    //    val cg = SignatureBasedCallGraph(apk, apk.model.getComponentInfos.flatMap(apk.getEntryPoints), None)
    val lol1 = apk.getApplicationClasses.map(x => x.getType.getPackageName)
    val lol2 = apk.model.getPackageName
    val lol3 = apk.getApplicationClasses.filter(x => x.getType.getPackageName == apk.model.getPackageName)
    val libs = new DefaultLibraryAPISummary(AndroidGlobalConfig.settings.third_party_lib_file)
    val libs1 = new DefaultLibraryAPISummary("./thirdpartylibs.txt")

    val lol4 = apk.getApplicationClasses.filter(x =>  libs.isLibraryClass(x.getType))
    analyseLibrary(apk)
    println()
    val orderedWUs: IList[Option[IntentWu]] = callGraph.topologicalSort(true).map { sig =>
      //      val method = apk.getMethodOrResolve(sig).getOrElse(throw new RuntimeException("Method does not exist: " + sig))
      //      new IntentWu(apk, method, sm, handler, store, "intent")

      apk.getMethodOrResolve(sig) match {
        case Some(method) => Some(new IntentWu(apk, method, sm, handler, store, "intent"))
        case None =>
          println("Removing from intent collection")
          println("Method " + sig)
          None
      }
    }

    analysis.build(orderedWUs.flatten)
  }

  def analyseLibrary(apk : ApkGlobal): Unit ={
    val allPackageNames = apk.getApplicationClasses.map(x => x.getType.getPackageName)
    val potentialLibraries = allPackageNames.filter(x => x != apk.model.getPackageName)
    println("Found libraries in for the following app " + apk.model.getPackageName)
    potentialLibraries.foreach(println)
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
            addToGraph(context.getMethodSig, context.getMethodSig.classTyp, new JawaType(name), context.getMethodSig.methodName, apk)
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
                        addToGraph(context.getMethodSig, context.getMethodSig.classTyp, classType, context.getMethodSig.methodName + ";" + f, apk)
                      }
                    })
                  }
                })
            }
          })
        }
    }
    collectIccByInits(apk)
  }

  def getClassName(method: JawaType, apk: ApkGlobal): String = {
    val methodClass = apk.getClazz(method).get
    if (methodClass.isInnerClass) {
      methodClass.getOuterType.get.jawaName
    } else {
      method.jawaName
    }
  }

  def addToGraph(parent: Signature, sourceType: JawaType, targetType: JawaType, method: String, apk: ApkGlobal): Unit = {
    val source = getClassName(sourceType, apk)
    val target = getClassName(targetType, apk)
    graph.getOrElseUpdate((source, target, method), msetEmpty)
    iccMethods.getOrElseUpdate(parent, (source, target, method))
  }

  def collectIccByInits(apk: ApkGlobal): Unit = {

    val iccClasses = iccMethods.keySet.map(x => x.classTyp)

    val entryPoints = apk.model.getComponentInfos.flatMap(apk.getEntryPoints)


    entryPoints.foreach(entryPoint => {
      //all methods
      val m = apk.getMethod(entryPoint).get


      val reachableMethods = callGraph.getReachableMethods(Set(m.getSignature))
      val reachableClasses = reachableMethods.groupBy(x => x.classTyp).filter(x => x._1 != m.getDeclaringClass.getType)
      iccClasses.foreach(j => {
        val iccClass = apk.getClazz(j).get

        //reachable contains one of the icc methods

        // We dont care for inner class calling outerclass
        if (iccClass.isInnerClass) {
          getReachableIccClasses(reachableClasses.keySet, iccClass.getOuterType.get, m)
        } else {
          getReachableIccClasses(reachableClasses.keySet, iccClass.getType, m)
        }

      })
    })
  }
}
