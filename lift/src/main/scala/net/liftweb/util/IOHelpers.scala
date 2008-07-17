package net.liftweb.util
import java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader, File, FileInputStream, BufferedReader, InputStreamReader}
import scala.collection.mutable.{HashSet, ListBuffer}

trait IoHelpers {
  def exec(cmds: String*): Can[String] = {
    try {
      class ReadItAll(in: InputStream, done: String => Unit) extends Runnable {
        def run {
          val br = new BufferedReader(new InputStreamReader(in))
          val lines = new ListBuffer[String]
          var line = ""
          while (line != null) {
            line = br.readLine
            if (line != null) lines += line
          }
          done(lines.mkString("\n"))
        }
      }
      
      var stdOut = ""
      var stdErr = ""
      val proc = Runtime.getRuntime.exec(cmds.toArray)
      val t1 = new Thread(new ReadItAll(proc.getInputStream, stdOut = _))
      t1.start
      val t2 = new Thread(new ReadItAll(proc.getErrorStream, stdErr = _))
      val res = proc.waitFor
      t1.join
      t2.join
      if (res == 0) Full(stdOut)
      else Failure(stdErr, Empty, Nil)
    } catch {
      case e => Failure(e.getMessage, Full(e), Nil)
    }
  }
    def readWholeThing(in: Reader): String = {
    val bos = new StringBuilder
    val ba = new Array[Char](4096)
    
    def readOnce {
      val len = in.read(ba)
      if (len < 0) return
      if (len > 0) bos.append(ba, 0, len)
      readOnce
    }
    
    readOnce
    
    bos.toString
  }

  def readWholeFile(file: File): Array[Byte] = readWholeStream(new FileInputStream(file))
  
  def readWholeStream(in: InputStream): Array[Byte] = {
    val bos = new ByteArrayOutputStream
    val ba = new Array[Byte](4096)
    
    def readOnce {
      val len = in.read(ba)
      if (len > 0) bos.write(ba, 0, len)
      if (len >= 0) readOnce
    }
    
    readOnce
    
    bos.toByteArray
  }
  


}
