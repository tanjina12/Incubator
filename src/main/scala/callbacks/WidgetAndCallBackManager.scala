package callbacks

import org.argus.amandroid.core.ApkGlobal
import org.argus.jawa.ast.Location
import org.argus.jawa.core._
import org.argus.jawa.core.util._


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
