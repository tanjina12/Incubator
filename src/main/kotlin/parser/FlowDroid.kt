package parser

import soot.Scene
import soot.jimple.infoflow.android.InfoflowAndroidConfiguration
import soot.jimple.infoflow.android.SetupApplication
import soot.jimple.infoflow.android.callbacks.FastCallbackAnalyzer
import soot.jimple.infoflow.android.manifest.ProcessManifest
import soot.jimple.toolkits.callgraph.CallGraph

class FlowDroid {

    var locationProtoBuf: String = "./ic3/protobuf"
    var locationApk: String = "./apps/app-debug.apk"
//    var locationApk: String = "./ic3/Pathe.apk"

    init {
        val manifest = ProcessManifest(locationApk)
        val packageName = manifest.packageName
        locationProtoBuf = "$locationProtoBuf/${packageName}_1.txt"

        Run("C:/Users/SG10/AppData/Local/Android/Sdk/platforms", locationApk, locationProtoBuf, packageName)
    }

    fun Run(androidSdkPath: String, apk: String, IC3Model: String, packageName: String) {
        val app = SetupApplication(androidSdkPath, apk)
        app.config.isTaintAnalysisEnabled = false
        app.config.mergeDexFiles = false
//        app.config.iccConfig.iccModel = IC3Model

        app.constructCallgraph()

        val callGraph = Scene.v().callGraph

        val l = FastCallbackAnalyzer(InfoflowAndroidConfiguration(), app.entrypointClasses)
        l.collectCallbackMethods()
        print("")
//        printActivityOnly(callGraph, packageName)
    }

    fun printFullTree(cg: CallGraph, packageName: String) {
        val edges = cg.listener()
        while (edges.hasNext()) {
            val e = edges.next()
            val src = e.src.method()
            val tgt = e.tgt.method()
            println("${src.signature} -- ${tgt.signature}")
        }
    }


    fun printAppTree(cg: CallGraph, packageName: String) {

        val edges = cg.listener()
        while (edges.hasNext()) {
            val e = edges.next()
            val src = e.src.method()
            val tgt = e.tgt.method()
            if (src.declaringClass.packageName == packageName || src.declaringClass.name == "IpcSC") {
                if (tgt.declaringClass.name == "IpcSC" || tgt.declaringClass.packageName == packageName) {
                    println("${src.signature} -ICC-> ${tgt.signature}")
                }

            }

        }
    }

    fun printActivityOnly(cg: CallGraph, packageName: String) {
        val links = mutableSetOf<Pair<String, String>>()
        val edges = cg.listener()
        while (edges.hasNext()) {
            val e = edges.next()
            val src = e.src.method()
            val tgt = e.tgt.method()

            if (src.declaringClass.name == "IpcSC") {

                val edgesOutSources = cg.edgesInto(src)
                while (edgesOutSources.hasNext()) {
                    val edge = edgesOutSources.next()
                    if (edge.tgt().declaringClass.name == "IpcSC") {
                        if (links.add(Pair(edge.src().signature, tgt.signature))) {
                            println("${edge.src().declaringClass.name} -ICC-> ${tgt.declaringClass.name}")
                            var label = edge.src().name
                        }
                    }
                }
            }
        }
    }
}