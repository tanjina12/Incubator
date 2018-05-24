package iccparser

import org.argus.amandroid.core.ApkGlobal
import org.argus.amandroid.core.model.Intent
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.core.Signature
import org.argus.jawa.core.util._

class DotGraphModel(cg: CallGraph = null) {
  val callerGraph: MSet[(Signature, Intent)] = msetEmpty
  val calleeGraph: MSet[(Signature, Intent)] = msetEmpty
  val t: MSet[(String, String, String)] = msetEmpty
  val endStringRegex = "[$1-9]+$"

  def addCallerTarget(signature: Signature, intent: Intent): Unit = {
    callerGraph.add(signature, intent)
  }

  def addCalleeTarget(signature: Signature, intent: Intent): Unit = {
    calleeGraph.add(signature, intent)
  }

  def add(signature: Signature, intents: ISet[Intent], caller: Boolean = true): Unit = {
    if (caller) {
      intents.foreach(x => addCallerTarget(signature, x))

    } else {
      intents.foreach(x => addCallerTarget(signature, x))

    }
  }

  def buildActivityGraph: MSet[(String, String, String)] = {
    callerGraph.foreach { x =>
      var target = ""
      if (x._2.componentNames.nonEmpty) {
        target = x._2.componentNames.head.replaceAll(endStringRegex, "")
      } else if (calleeContains(x._2.actions.head)) {
        target = x._2.actions.head.replaceAll(endStringRegex, "")
      } else {
        return t
      }

      val source = x._1.getClassName.replaceAll(endStringRegex, "")
      val label = x._1.methodName
      t.add(source, target, label)
    }

    return t
  }

  def buildFullAppGraph(apk: ApkGlobal): MSet[(String, String)] = {
    val callGraph: MSet[(String, String)] = msetEmpty

    callerGraph.foreach { s =>
      if (s._2.componentNames.nonEmpty) {
        val sig = getSignature(s._2.componentNames.head)
        cg.addCall(s._1, sig)
      }
    }

    cg.getCallMap.foreach { c =>
      c._2.foreach { p =>
        if (p.classTyp.getPackageName == apk.model.getPackageName || p.methodName == "dummyInit") {
          callGraph += ((c._1.toString(), p.signature.toString))
        }
      }
    }
    return callGraph
  }

  def buildSimplifiedAppGraph(apk: ApkGlobal): MSet[(String, String, String)] = {
    val graph: MSet[(String, String, String)] = msetEmpty

    cg.getCallMap.foreach { c =>
      c._2.foreach { p =>
        val source = c._1.getClassName.replaceAll(endStringRegex, "")
        val target = p.getClassName.replaceFirst(endStringRegex, "")
        if (source != target) {
          if (p.classTyp.getPackageName == apk.model.getPackageName || p.methodName == "dummyInit") {
            graph += ((source, target, c._1.methodName))
          }
        }
      }
    }
    return graph
  }

  private def getSignature(className: String): Signature = {
    new Signature(s"${className.replace(".", "/")};.dummyInit:()V")
  }

  def calleeContains(actionIntent: String): Boolean = {
    calleeGraph.exists(x => x._2.actions.nonEmpty && x._2.actions.head == actionIntent)
  }
}
