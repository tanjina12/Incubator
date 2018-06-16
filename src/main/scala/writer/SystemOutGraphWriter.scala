package writer

class SystemOutGraphWriter extends BaseGraphWriter {
  override def write(graph: List[(String, String, String)], packageName: String): Unit = {
    graph.foreach {
      case (source, target, method) =>
        val line: String = s" $source -ICC-> $target  by method $method]"
        println(line)
    }
  }
}
