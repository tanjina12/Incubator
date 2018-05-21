import com.natpryce.konfig.ConfigurationProperties
import config.incubator
import soot.Scene
import soot.jimple.infoflow.android.SetupApplication
import soot.jimple.infoflow.android.manifest.ProcessManifest
import soot.jimple.toolkits.callgraph.CallGraph
import soot.util.dot.DotGraph
import java.io.File
import java.nio.file.StandardOpenOption


fun main(args: Array<String>) {
    println("Starting incubator")
    val config = ConfigurationProperties.fromFile(File("incubator.properties"))

    val androidSdkPath = config[incubator.androidSdkPath]

    File(config[incubator.apk]).walk().forEach {
        if (it.extension == "apk"){

            val manifest = ProcessManifest(it)
            val packageName = manifest.packageName
            println(it)

            val ic3Dir = File("${config[incubator.apkIC3File]}/$packageName/")

            if (ic3Dir.exists()){
                val ic3Result = ic3Dir.listFiles().first()
                println(ic3Result)

                try {
                    run(androidSdkPath, it.toString(), ic3Result.toString(), packageName)
                }catch (e : Exception){
                    println(e.printStackTrace())
                    println("Failed $packageName skipping..")
                    val errorFile = File("errors.txt")
                    errorFile.writeText("Failed $packageName skipping..")
                    errorFile.writeText(e.printStackTrace().toString())
                }
            }
        }
    }

}

fun run(androidSdkPath: String, apk : String, IC3Model : String, packageName: String){
    val app = SetupApplication(androidSdkPath, apk)
    app.config.isTaintAnalysisEnabled = false
    app.config.mergeDexFiles = true
    app.config.iccConfig.iccModel = IC3Model

    app.constructCallgraph()


    val callGraph = Scene.v().callGraph

    printFullTree(callGraph, packageName)
    printActivityOnly(callGraph, packageName)
    printAppTree(callGraph, packageName)
}

fun printFullTree(cg: CallGraph, packageName: String) {
    val dg = DotGraph("Call Graph")
    val edges = cg.listener()
    while (edges.hasNext()) {
        val e = edges.next()
        val src = e.src.method()
        val tgt = e.tgt.method()
        dg.drawEdge(src.signature, tgt.signature)
    }

    dg.plot("./output/${packageName}_fullgraph.dot")
}

fun printAppTree(cg: CallGraph, packageName: String) {
    val dg = DotGraph("Call Graph")
    val edges = cg.listener()
    while (edges.hasNext()) {
        val e = edges.next()
        val src = e.src.method()
        val tgt = e.tgt.method()
        if (src.declaringClass.packageName == packageName || src.declaringClass.name == "IpcSC"  ) {
            if (tgt.declaringClass.name == "IpcSC" || tgt.declaringClass.packageName == packageName){
                dg.drawEdge(src.signature, tgt.signature)
            }
        }
    }

    dg.plot("./output/${packageName}_appgraph.dot")
}

fun printActivityOnly(cg: CallGraph, packageName: String) {
    val links = mutableSetOf<Pair<String, String>>()
    val dg = DotGraph("Call Graph")
    val edges = cg.listener()
    while (edges.hasNext()) {
        val e = edges.next()
        val src = e.src.method()
        val tgt = e.tgt.method()

        if (src.declaringClass.name == "IpcSC") {

            val edgesOutSources = cg.edgesInto(src)
            while (edgesOutSources.hasNext()){
                val edge = edgesOutSources.next()
                if (edge.tgt().declaringClass.name == "IpcSC"){
                    if(links.add(Pair(edge.src().signature, tgt.signature))){
                        val dgEdge = dg.drawEdge(edge.src().declaringClass.name, tgt.declaringClass.name)
//                        val dgEdge = dg.drawEdge(edge.src().signature, tgt.signature)
                        dgEdge.setLabel(edge.src().name)
                    }
                }
            }
        }

        dg.plot("./output/${packageName}_actvitygraph.dot")
    }
}
