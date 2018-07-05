import com.github.javaparser.printer.concretesyntaxmodel.CsmElement.block
import com.google.common.util.concurrent.Futures.withTimeout
import com.natpryce.konfig.ConfigurationProperties
import config.Mode
import java.io.File
import config.incubator
import kotlinx.coroutines.experimental.async
import kotlinx.coroutines.experimental.delay
import kotlinx.coroutines.experimental.runBlocking
import kotlinx.coroutines.experimental.withTimeout
import org.apache.commons.brut.io.FileUtils
import parser.FlowDroid
import java.io.FileOutputStream
import java.io.IOException
import java.io.PrintWriter
import java.util.concurrent.TimeUnit

fun main(args: Array<String>) {
    println("Starting incubator")
    val config = ConfigurationProperties.fromFile(File("incubator.properties"))
    val basePathResult = config[incubator.output]
//    val mode = config[incubator.mode]
    val ignoreAppsFile = config[incubator.ignoreApps]
//    val parseInnerClassToOuterClass = config[incubator.parseInnerClassAdOuterClass]

    var ignoreApps = File(ignoreAppsFile).readLines().toSet()
//    ignoreApps += File("timeout.txt").readLines().toSet()

    File(config[incubator.apk]).walk().forEach {
        if (it.extension == "apk" && !ignoreApps.contains(it.toString())) {
            println(it)

            if (!File(basePathResult).exists()) {
                File(basePathResult).mkdir()
            }

//            runFlowDroid()
            runArgus(it, basePathResult)
        }
    }


}

fun runFlowDroid() {
    FlowDroid()
}

fun runArgus(app: File, basePathResult: String) = runBlocking<Unit> {
    try {

        val d = async { Argus().run(app.toString(), basePathResult) } // run the block code in background
        withTimeout(60, TimeUnit.MINUTES) { d.await() } // wait with timeout

        val writer = PrintWriter(FileOutputStream(File("succes.csv"), true))
        writer.append(app.toString() + "\r\n")
        writer.close()

        if (File("./output").isDirectory) {
            FileUtils.deleteDirectory(File("./output"))
        }
    } catch (e: IOException) {
    } catch (e: Exception) {
        println(e.printStackTrace())
        println("Failed skipping.. $app")
        val errorFile = File("errors.txt")
        val writer = PrintWriter(FileOutputStream(errorFile, true))
        writer.append("Failed skipping.. $app \r\n")
        e.printStackTrace(writer)
        writer.close()
    }
}