package writer

import java.io.Writer

import org.argus.jawa.core.JawaMethod

import scala.tools.nsc.classpath.PackageNameUtils

class MethodWriter(writer: Writer) {
  def write(methods: Set[JawaMethod]): Unit = {
    writer.write("package,class,method\r\n")
    methods.foreach { method =>
      val (packageName, class_name) = PackageNameUtils.separatePkgAndClassNames(method.declaringClass.getName)
      println(s"$class_name methods ${method.name}")
      val line = "%s,%s,%s\r\n".format(packageName,method.declaringClass.getName, method.name)
      writer.write(line)
    }
    writer.close()
  }
}
