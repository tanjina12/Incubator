package writer

import java.io.Writer

import org.argus.jawa.alir.cg.CallGraph

class MethodWriter(writer : Writer) {
  def write(callGraph: CallGraph): Unit ={
    writer.write("Class,Method\r\n")
    callGraph.getCallMap.foreach{
      case (className,_) =>
        println(s"${className.getClassName} methods ${className.methodName}")
        val line = "%s,%s\r\n".format(className.getClassName, className.methodName)
        writer.write(line)
    }
    writer.close()
  }
}
