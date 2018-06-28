package writer

import java.io.Writer

import org.argus.jawa.core.util.{MMap, MSet}

class XmlGraphWriter(writer: Writer) extends BaseGraphWriter {
  override def write(graph: MMap[(String, String, String), MSet[String]], packageName: String): Unit = {
    writer.write(s"<$packageName>$lineSeparator")
    graph.foreach {
      case ((source, target, method), widgets) =>
        widgets.foreach(widget => {
          writer.write(s"<ICC>$lineSeparator")
          writer.write(s"<source>$source</source>$lineSeparator")
          writer.write(s"<target>$target</target>$lineSeparator")
          writer.write(s"<method>$method</method>$lineSeparator")
          writer.write(s"<widget>$widget</widget>$lineSeparator")
          writer.write(s"</ICC>$lineSeparator")
        })
    }
    writer.write(s"</$packageName>")
    writer.close()
  }
}
