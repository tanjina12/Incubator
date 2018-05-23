package iccparser

import org.argus.amandroid.core.ApkGlobal
import org.argus.amandroid.core.model.Intent
import org.argus.jawa.alir.cg.CallGraph
import org.argus.jawa.core.Signature
import org.argus.jawa.core.util._

class DotGraphModel(cg : CallGraph = null) {
  val callerGraph: MSet[(Signature, Intent)] = msetEmpty
  val calleeGraph: MSet[(Signature, Intent)] = msetEmpty
  val t: MSet[(String, String, String)] = msetEmpty

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
        target = x._2.componentNames.head
      } else if (calleeContains(x._2.actions.head)) {
        target = x._2.actions.head
      }else{
        return t
      }

      val source = x._1.getClassName
      val label = x._1.methodName
      t.add(source, target, label)
    }

    return t
  }

  def buildFullAppGraph(apk: ApkGlobal): MList[(String, String)] = {
    val callGraph: MList[(String, String)] = mlistEmpty

    callerGraph.foreach { s =>
      val sig = getSignature(s._1.getClassName)
      cg.addCall(s._1, sig)
    }

    cg.getCallMap.foreach { c =>
      callGraph += ((c._1.getClassName, c._1.toString()))
      c._2.foreach { p =>
        if (c._1.getClassName != p.getClassName) {
          if (p.classTyp.getPackageName == apk.model.getPackageName) {
            callGraph += ((c._1.toString(), p.signature.toString))
          }
        }
      }
    }
    return callGraph
  }

  private def getSignature(className: String): Signature = {
    new Signature(s"${className.replace(".", "/")};.dummyInit:()V")
  }

  def calleeContains(actionIntent : String): Boolean ={
    calleeGraph.exists(x => x._2.actions.nonEmpty && x._2.actions.head == actionIntent)
  }
}
