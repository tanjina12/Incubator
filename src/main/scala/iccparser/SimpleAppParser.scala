package iccparser

import java.io.{PrintWriter, Writer}

import org.argus.amandroid.alir.componentSummary.ApkYard
import org.argus.amandroid.core.ApkGlobal
import org.argus.jawa.core.util.MSet

class SimpleAppParser(apk: ApkGlobal, yard: ApkYard, var writer: Writer = new PrintWriter(System.out)) extends BaseAppParser(apk, yard, writer) {
  override def run: Unit = {
    val dotGraphModel = Intent()
    val dotLayout: MSet[(String, String, String)] = dotGraphModel.buildSimplifiedAppGraph(apk)
    writeGraph(dotLayout)
  }

  override def run(dotGraphModel: DotGraphModel, writer: Writer): Unit = {
    this.writer = writer
    val dotLayout: MSet[(String, String, String)] = dotGraphModel.buildSimplifiedAppGraph(apk)
    writeGraph(dotLayout)
  }

  private def writeGraph(dotLayout: MSet[(String, String, String)]): Unit = {

    writer.write("digraph G {" + lineSeparator)
    println("digraph G {")
    dotLayout.foreach { x =>
      println("\"" + x._1 + "\" -> \"" + x._2 + "\"[label=" + x._3 + "]")
      writer.write("\"" + x._1 + "\" -> \"" + x._2 + "\"[label=" + x._3 + "]" + lineSeparator)
    }
    apk.model.getActivities.foreach {
      x =>
        println("\"" + x.name + "\"")
        writer.write("\"" + x.name + "\"" + lineSeparator)
    }
    writer.write("}" + lineSeparator)
    println("}")
    writer.close()
  }
}
