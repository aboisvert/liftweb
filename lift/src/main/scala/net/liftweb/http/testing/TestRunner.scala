package net.liftweb.http.testing;


import net.liftweb.util.Helpers._
import net.liftweb.util.Helpers
import scala.collection.mutable.ArrayBuffer

class TestRunner(clearDB: () => Any, setupDB: () => Any) {
  private var _assertCnt = 0
  var beforeAssertListeners: List[String => Any] = Nil
  var afterAssertListeners: List[(String, Boolean) => Any] = Nil
  private var log = new ArrayBuffer[Tracker]()
 
  implicit def fToItem(f: () => Any): Item = Item(f)
  
  def beforeAssert(name: String): unit = synchronized {
    _assertCnt = _assertCnt + 1
    log += Tracker(name, true, true, true, None, Nil)
    beforeAssertListeners.foreach(_(name))
  }
  
  def afterAssert(name: String, success: Boolean): unit = synchronized {

    log += Tracker(name, true, false, success, None, Nil)
    afterAssertListeners.foreach(_(name, success))    
  }
  
    def applyAssert[T](name: String)(f: => T) = {
    var success = false
    beforeAssert(name)
    try {
      val ret = f
      success = true
      ret
    } finally {
      afterAssert(name, success)
    }
  }
    
    def doResetDB {
      clearDB()
      setupDB()
    }
    
    
    
    def run(what: List[Item]): TestResults = {
      clearDB()
      setupDB()
      _assertCnt = 0
      log = new ArrayBuffer()
      what.foreach{
        testItem =>
        log += Tracker(testItem.name, false, true, true, None, Nil)
        val myTrace = (try{throw new Exception("")} catch {case e => e}).getStackTrace.toList.tail.head
        if (testItem.resetDB) doResetDB
        val (success, trace: List[StackTraceElement], excp: Option[Throwable]) = try {
          testItem.func()
          (true, Nil, None)
        } catch {
          case e => 
          def combineStack(ex: Throwable,base: List[StackTraceElement]): List[StackTraceElement] = ex match {
            case null => base
            case e => combineStack(e.getCause,e.getStackTrace.toList ::: base)
          }
          val trace = combineStack(e, Nil).takeWhile(e => e.getClassName != myTrace.getClassName || e.getFileName != myTrace.getFileName || e.getMethodName != myTrace.getMethodName).dropRight(2)
          (false, trace, Some(e))
        }
        
        log += Tracker(testItem.name, false, false, success, excp, trace)       
      }
      TestResults(log.toList)
    }
}

case class TestResults(res: List[Tracker]) {
  def stats = {
    val rev = res.reverse
    val start = res.map(_.at).reduceLeft((a: long,b: long) => if (a < b) a else b)
    val end = res.map(_.at).reduceLeft((a: long,b: long) => if (a > b) a else b)
    val assertCnt = res.filter(a => a.isAssert && !a.isBegin).length
    val testCnt = res.filter(a => a.isTest && !a.isBegin).length
    val failedAsserts = res.filter(a => a.isAssert && !a.success)
    val failedTests = res.filter(a => a.isTest && !a.success)
    
    val append = (failedTests, failedAsserts) match {
      case (ft,fa) if ft.length == 0 && fa.length == 0 => ""
      case (ft, fa) => 
        "\n"+ft.length+" Failed Tests:\n"+ft.map(v => v.name+" "+v.exception.get.getMessage+" \n"+
          v.trace.map(st => "           "+st.toString).mkString("\n")).mkString("\n")
    }
    
    "Ran "+testCnt+" tests and "+assertCnt+" asserts in "+(end - start)/1000L+" seconds"+append 
  }
}

class TestFailureError(msg: String) extends Error(msg)

class Item(val name: String, val resetDB: Boolean, val func: () => Any)
object Item {
  private var _cnt = 0
  private def cnt() = synchronized {
    _cnt = _cnt + 1
    _cnt
  }
  def apply(f: () => Any) = new Item("Test "+cnt(), false, f)
  def apply(name: String, f: () => Any) = new Item(name, false, f)
  def apply(name: String, resetDB: Boolean, f: () => Any) = new Item(name, resetDB, f)
}

case class Tracker(name: String, isAssert: Boolean, isBegin: Boolean, success: Boolean,
    exception: Option[Throwable], trace: List[StackTraceElement]) {
  val at = millis
  def isTest = !isAssert
}


