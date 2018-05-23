import com.natpryce.konfig.ConfigurationProperties
import java.io.File
import config.incubator


fun main(args: Array<String>) {
    println("Starting incubator")
    val config = ConfigurationProperties.fromFile(File("incubator.properties"))
    val basePathResult = config[incubator.output]
    val mode = config[incubator.mode]
    File(config[incubator.apk]).walk().forEach {
        if (it.extension == "apk") {
            println(it)

            if (!File(basePathResult).exists()) {
                File(basePathResult).mkdir()
            }

            try {
                Argus().run(it.toString(), basePathResult, mode.toString())
            } catch (e: Exception) {
                println(e.printStackTrace())
                println("Failed skipping..")
                val errorFile = File("errors.txt")
                errorFile.writeText("Failed skipping..")
                errorFile.writeText(e.printStackTrace().toString())
            }

        }
    }
}