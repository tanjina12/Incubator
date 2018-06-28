package writer

import org.argus.jawa.core.util.{MMap, MSet}

class SystemOutGraphWriter extends BaseGraphWriter {
  override def write(graph: MMap[(String, String, String), MSet[String]], packageName: String): Unit = {
    graph.foreach {
      case ((source, target, method), widgets) =>
        widgets.foreach(widget => {
          val line: String = s" $source -ICC-> $target  by method $method and widget $widget"
          println(line)
        })
    }
  }
}
