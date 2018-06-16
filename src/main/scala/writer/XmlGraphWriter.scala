package writer

import java.io.Writer

class XmlGraphWriter(writer: Writer) extends BaseGraphWriter {
  override def write(graph: List[(String, String, String)], packageName: String): Unit = {
    writer.write(s"<$packageName>$lineSeparator")
    graph.foreach {
      case (source, target, method) =>
        writer.write("<ICC>")
        writer.write(s"<source>$source</source>$lineSeparator")
        writer.write(s"<target>$target</target>$lineSeparator")
        writer.write(s"<method>$method</method>$lineSeparator")
        writer.write("</ICC>")
    }
    writer.write(s"</$packageName>")

  }
}
