package writer

import org.argus.jawa.core.util.{MMap, MSet}

trait BaseGraphWriter {
  val lineSeparator = "\r\n"
  def write(graph: MMap[(String, String, String), MSet[String]], packageName : String)
}
