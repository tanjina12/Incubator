package iccparser

import java.io.{PrintWriter, Writer}

import org.argus.amandroid.alir.componentSummary.ApkYard
import org.argus.amandroid.core.ApkGlobal
import org.argus.jawa.core.util.MList

class FullAppParser(apk: ApkGlobal, yard: ApkYard, writer: Writer = new PrintWriter(System.out)) extends BaseAppParser(apk, yard, writer) {
  override def run: Unit = {
    val dotGraphModel = Intent()
    val dotLayout = dotGraphModel.buildFullAppGraph(apk)
    writeGraph(dotLayout)
  }

  private def writeGraph(dotLayout: MList[(String, String)]): Unit = {

    writer.write("digraph G {" + lineSeparator)
    dotLayout.foreach { x =>
      writer.write("\"" + x._1 + "\" -> \"" + x._2 + "\"" + lineSeparator)
    }
    apk.model.getActivities.foreach {
      x =>
        writer.write("\"" + x.name + "\"" + lineSeparator)
    }
    writer.write("}" + lineSeparator)
    writer.close()
  }
}
