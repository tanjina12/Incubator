import com.natpryce.konfig.ConfigurationProperties
import hu.ssh.progressbar.console.ConsoleProgressBar
import org.argus.amandroid.alir.componentSummary.ApkYard
import org.argus.amandroid.core.AndroidGlobalConfig
import org.argus.amandroid.core.ApkGlobal
import org.argus.amandroid.core.decompile.DecompileLayout
import org.argus.amandroid.core.decompile.DecompileLevel
import org.argus.amandroid.core.decompile.DecompileStrategy
import org.argus.amandroid.core.decompile.DecompilerSettings
import org.argus.jawa.alir.pta.suspark.InterProceduralSuperSpark
import org.argus.jawa.core.DefaultLibraryAPISummary
import org.argus.jawa.core.MsgLevel
import org.argus.jawa.core.PrintReporter
import org.argus.jawa.core.Signature
import org.argus.jawa.core.util.FileUtil
import scala.Option
import scala.collection.immutable.Seq
import scala.collection.immutable.Set
import java.io.File


fun main(args: Array<String>) {
    println("Starting incubator")
    val config = ConfigurationProperties.fromFile(File("incubator.properties"))


//    val apkUri = FileUtil.toUri("app.apk")
//    val outputUri = FileUtil.toUri("./output")
//    val reporter = PrintReporter(MsgLevel.ERROR())
//    val yard = ApkYard(reporter)
//    val layout = DecompileLayout(outputUri, true, "src", "lib", true)
//    val consoleBar = ConsoleProgressBar.on(System.out).withFormat("[:bar] :percent% :elapsed ETA: :eta")
//    val strategy = DecompileStrategy(layout, DefaultLibraryAPISummary(AndroidGlobalConfig.settings().third_party_lib_file()), DecompileLevel.UNTYPED(), DecompileLevel.SIGNATURE(), true)
//
//    val settings = DecompilerSettings(false, true, strategy, reporter, Option.empty(), 15, consoleBar)
//
//    val apk : ApkGlobal = yard.loadApk(apkUri, settings, true, true, false)
//
//
//    val name = apk.model().appName
//    println("Name of app $name")
//    println("Activities ${apk.model().activities}")
//    println("Appname ${apk.model().appName}")
//    println("CB methods ${apk.model().callbackMethods}")
//    apk.model().components.foreach{ svc ->
//        println(svc)
//        val clz = apk.getClassOrResolve(svc)
//        val eps = clz.declaredMethods
//        val spark = InterProceduralSuperSpark(apk)
//        val l : Set<Signature>
//
//        l = eps.map()
//        val idfg = spark.build(eps.map(getSignature))
//        println(idfg.ptaresult())
//    }

    Argus().run()


}