package writer

import java.io.Writer

import org.argus.jawa.core.util.{MMap, MSet}

class DotGraphWriter(writer: Writer) extends BaseGraphWriter {
  override def write(graph: MMap[(String, String, String), MSet[String]], packageName: String): Unit = {
    writer.write(s"digraph $packageName {$lineSeparator")
    graph.foreach {
      case ((source, target, method), widgets) =>
        widgets.foreach(widget => {
          val line: String = "\"" + source + "\" -> \"" + target + "\" [label=" + method + ";" + widget + "]" + lineSeparator
          writer.write(line)
        })
    }
    writer.write("}")
    writer.close()
  }
}
