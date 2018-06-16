package writer

trait BaseGraphWriter {
  val lineSeparator = "\r\n"
  def write(graph: List[(String, String, String)], packageName : String)
}
