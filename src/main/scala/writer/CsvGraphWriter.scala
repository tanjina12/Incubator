package writer

import java.io.Writer


class CsvGraphWriter(writer: Writer) extends BaseGraphWriter {
  override def write(graph: List[(String, String, String)], packageName: String): Unit = {
    writer.write(s"package,source,target,method$lineSeparator")
    graph.foreach {
      case (source, target, method) =>
        val line: String = s"$packageName,$source,$target,$method$lineSeparator"
        writer.write(line)
    }
    writer.close()
  }
}
