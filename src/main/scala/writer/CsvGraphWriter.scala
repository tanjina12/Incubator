package writer

import java.io.Writer

import org.argus.jawa.core.util.{MMap, MSet}


class CsvGraphWriter(writer: Writer) extends BaseGraphWriter {
  override def write(graph: MMap[(String, String, String), MSet[String]], packageName: String): Unit = {
    writer.write(s"package,source,target,method,widget$lineSeparator")
    graph.foreach {
      case ((source, target, method), widgets) =>
        widgets.foreach(widget =>{
          val line: String = s"$packageName,$source,$target,$method,$widget$lineSeparator"
          writer.write(line)
        })
    }
    writer.close()
  }
}
