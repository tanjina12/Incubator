package writer

import java.io.Writer

class DotGraphWriter(writer: Writer) extends BaseGraphWriter {
  override def write(graph: List[(String, String, String)], packageName: String): Unit = {
    writer.write(s"digraph $packageName {$lineSeparator")
    graph.foreach {
      case (source, target, method) =>
        val line: String = "\"" + source + "\" -> \""+ target +"\" [label="+ method+"]"+lineSeparator
        writer.write(line)
    }
    writer.write("}")
    writer.close()
  }
}
