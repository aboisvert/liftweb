package net.liftweb.util

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.net.{URLDecoder, URLEncoder}
import scala.collection.mutable.{HashSet}
import scala.xml.{NodeSeq, Elem, Node}
import scala.collection.{Map}
import scala.collection.mutable.HashMap
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import java.util.Date
import java.text.SimpleDateFormat
import java.lang.reflect.Modifier
import org.apache.commons.codec.binary.Base64
import java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader}
import java.security.{SecureRandom, MessageDigest}
import javax.mail._


/**
 *  A bunch of helper functions
 */
object Helpers {
  private var otherPackages: List[String] = Nil
  
  def buildPackage(end: String)  = synchronized {otherPackages.map{op => op+"."+end}}
  
  def addToPackages(what: String) {synchronized {otherPackages = what :: otherPackages}}
  
  /**
   * URL decode the string.  A pass-through to Java's URL decode with UTF-8
   */
  def urlDecode(in : String) = {URLDecoder.decode(in, "UTF-8")}
  def urlEncode(in : String) = {URLEncoder.encode(in, "UTF-8")}
  
  val validSuffixes = {
    val ret = new HashSet[String];
    // ret += "html" // serve html files using the servlet
    ret += "png"
    ret += "js"
    ret += "css"
    ret += "jpg"
    ret += "tiff"
    ret += "jpeg"
    ret
  }
  
  def goodPath_?(path : String) : boolean = {
    if (path == null || path.length == 0 || !path.startsWith("/") || path.indexOf("/.") != -1) false
    else {
      val lastPoint = path.lastIndexOf('.')
      val lastSlash = path.lastIndexOf('/')
      if (lastPoint <= lastSlash) false else {
	validSuffixes.contains(path.substring(lastPoint + 1))
      }
    }
  }
  
  def couldBeHtml(in : Map[String, String]) : boolean = {
    in match {
      case null => true
      case _ => {
	in.get("Content-Type") match {
	  case s @ Some(_) => {s.get.toLowerCase == "text/html"}
	  case None | null => true
	}
      }
    }
  }
  
  def noHtmlTag(in : NodeSeq) : boolean = {
    (in \\ "html").length != 1
  }
  
  def toHashMap[A,B](in : Map[A,B]) : HashMap[A,B] = {
    val ret = new HashMap[A,B];
    in.keys.foreach {
      k =>
	ret += k -> in(k)
    }
    
    ret
  }
  
  def first[B,C](in : Seq[B])(f : B => Option[C]) : Option[C] = {
    in match {
      case Nil => None
      case x :: xs => {
	f(x) match {
          case s @ Some(_) =>  s
          case None => first(xs)(f)
	}
      }
    }
  }
  
  
  def processBind(around : NodeSeq, at : String, what : NodeSeq) : NodeSeq = {
    around.flatMap {
      v =>
	v match {
	  case Elem("lift", "bind", attr @ _, _, kids @ _*) if (attr("name").text == at) => {what}
          case Elem(_,_,_,_,kids @ _*) => {Elem(v.prefix, v.label, v.attributes, v.scope, processBind(kids, at, what) : _*)}
          case _ => {v}
	}
      
    }
  }
  
  /**
   * Insure all the appropriate fields are in the header
   */
  def insureField(toInsure : Map[String, String], headers : List[Pair[String, String]]) : Map[String, String] = {
    def insureField_inner(toInsure : HashMap[String, String], field : Pair[String, String]) : HashMap[String, String] = {
      toInsure match {
        case null => {val ret = new HashMap[String, String]; ret += field._1 -> field._2; ret}
        case _ if (toInsure.contains(field._1)) => toInsure
        case _ => {toInsure += field._1 -> field._2; toInsure}
      }
    }

    if (headers.isEmpty) 
      if (toInsure == null) new HashMap else toInsure
      else insureField(insureField_inner(toHashMap(toInsure), 
					 headers.head), headers.tail) 

  }
  
  /**
   * Convert an enum to a List[T]
   */
  def enumToList[T](enum : java.util.Enumeration) : List[T] = {
    if (enum.hasMoreElements) {
      val next = enum.nextElement
      if (next.isInstanceOf[T]) next.asInstanceOf[T] :: enumToList[T](enum)
      else enumToList[T](enum)
    } else
      Nil
  }
  
  /**
   * Convert an enum to a List[String]
   */
  def enumToStringList(enum : java.util.Enumeration) : List[String] = {
    if (enum.hasMoreElements) enum.nextElement.toString :: enumToStringList(enum) else Nil
  }
  
  def head[T](l : List[T], deft: T) = if (l.isEmpty) deft else l.head
  
  /**
   * Find a class with name given name in a list of packages, either by matching 'name'
   * or by matching 'smartCaps(name)'
   */
  def findClass(name : String, where : List[String]) : Option[Class] = {
    findClass(name, where, List(smartCaps, {n => n}), {s => true})
  }
  
  /**
   * Find a class with name given name in a list of packages, either by matching 'name'
   * or by matching 'smartCaps(name)'
   */
  def findClass(name : String, where : List[String], guard: (Class) => boolean ) : Option[Class] = {
    findClass(name, where, List(smartCaps, {n => n}), guard)
  }
  
  def findClass(where : List[Pair[String, List[String]]]) : Option[Class] = {
    where match {
      case Nil => {null}
      case s :: rest => {
	findClass(s._1, s._2) match {
          case null => findClass(rest)
          case s @ _ => s
	}
      }
    }
  }

  /**
   * Find a class with name given name in a list of packages, with a list of functions that modify
   * 'name' (e.g., leave it alone, make it camel case, etc.)
   */
  def findClass(name : String, where : List[String], modifiers : List[Function1[String, String]], guard: (Class) => boolean) : Option[Class] = {
    def findClass_s(name : String, where : String) : Class = {
      tryn(List(classOf[ClassNotFoundException])) {
        val clzName = where+"."+name
	
        Class.forName(clzName)
      }
    }
    
    
    def findClass_l(name : String, where : List[String]) : Option[Class] = {
      where match {
        case Nil => {null}
        case c :: rest => {
          findClass_s(name, c) match {
            case null => {findClass_l(name, rest)}
            case r @ _ => {Some(r)}
            
          }
        }
      }
    }
    
    modifiers match {
      case Nil => {null}
      case c :: rest => {
        findClass_l(c(name), where) match {
          case null => {findClass(name, where, rest, guard)}
          case r @ _ => {r}
        }
      }
      //testn(findClass_l(c(name), where)) {findClass(name, where, rest)}}
    }
  }
  
  /**
   * Wraps a "try" block around the function f.  If f throws
   * an exception with it's class in 'ignore' or of 'ignore' is
   * null or an empty list, ignore the exception and return null.
   */
  def tryn[T](ignore : List[Class])(f : => T) : T = {
    try {
      f
    } catch {
      case c if (containsClass(c.getClass, ignore)) => {null.asInstanceOf[T]}
      case c if (ignore == null || ignore.isEmpty) => {null.asInstanceOf[T]}
    }
  }
  
  /**
   * Wraps a "try" block around the function f.  If f throws
   * an exception with it's class in 'ignore' or of 'ignore' is
   * null or an empty list, ignore the exception and return None.
   */
  def tryo[T](ignore : List[Class])(f : => T) : Option[T] = {
    try {
      Some(f)
    } catch {
      case c if (containsClass(c.getClass, ignore)) => {None}
      case c if (ignore == null || ignore.isEmpty) => {None}
    }
  }
  
  /**
   * Wraps a "try" block around the function f.  If f throws
   * an exception return None
   */
  def tryo[T](f: => T): Option[T] = {
    tryo(Nil)(f)
  }
  
  def callableMethod_?(meth : Method) = {
    meth != null && meth.getParameterTypes.length == 0 && (meth.getModifiers & java.lang.reflect.Modifier.PUBLIC) != 0
  }
  
  
  
  /**
   * Wraps a "try" block around the function f.  If f throws
   * an exception with it's class in 'ignore' or of 'ignore' is
   * null or an empty list, ignore the exception and return false.
   */
  def tryf[T](ignore : List[Class])(f : => T) : Any = {
    try {
      f
    } catch {
      case c if (containsClass(c.getClass, ignore)) => {false}
      case c if (ignore == null || ignore.isEmpty) => {false}
    }
  }
  
  /**
   * Wraps a "try" block around the function f.  If f throws
   * an exception, ignore the exception and return null.
   */
  def tryn[T](f : => T) : T = {tryn(null)(f)}
  
  /**
   * Is the clz an instance of (assignable from) any of the classes in the list
   * 
   * @param clz the class to test
   * @param toMatch the list of classes to match against
   * 
   * @return true if clz is assignable from of the matching classes
   */
  def containsClass(clz : Class, toMatch : List[Class]) : boolean = {
    toMatch match {
      case null => {false}
      case Nil => {false}
      case c :: rest if (c.isAssignableFrom(clz)) => {true}
      case _ => {false}
    }
  }
  
  def classHasControllerMethod(clz : Class, methName : String) = {
    tryn {
      clz match {
        case null => false
        case _ => callableMethod_?(clz.getMethod(methName, null))
      }
    }
  }

  def invokeControllerMethod(clz : Class, meth : String) = {
    try {
      clz.getMethod(meth, null).invoke(clz.newInstance, null)
    } catch {
      case c : InvocationTargetException => {def findRoot(e : Throwable) {if (e.getCause == null || e.getCause == e) throw e else findRoot(e.getCause)}; findRoot(c)}
    }
  }
  
  private def _invokeMethod(clz: Class, meth: String): Option[Any] = {
    try {
      clz.getMethod(meth, null) match {
	case null => None
	case m => Some(m.invoke(clz.newInstance, null))
      }
    } catch {
      case e: java.lang.NoSuchMethodException => None
    }
  }
  
  def invokeMethod(clz: Class, meth: String): Option[Any] = {
    _invokeMethod(clz,meth) orElse _invokeMethod(clz, smartCaps(meth)) orElse 
    _invokeMethod(clz, methodCaps(meth)) orElse None
  }
  
  def methodCaps(name: String): String = {
    val tmp = smartCaps(name)
    tmp.substring(0,1).toLowerCase + tmp.substring(1)
  }
  
  /**
   * Turns a string of format "foo_bar" into camel case "FooBar"
   *
   * Functional code courtesy of Jamie Webb (j@jmawebb.cjb.net) 2006/11/28 
   * @param in The String to CamelCase
   *
   * @return the CamelCased string
   */
  def smartCaps(in : String) = {
    def loop(x : List[Char]) : List[Char] = (x: @unchecked) match {
      case '_' :: '_' :: rest => loop('_' :: rest)
      case '_' :: c :: rest => Character.toUpperCase(c) :: loop(rest)
      case c :: rest => c :: loop(rest)
      case Nil => Nil
    }

    List.toString(loop('_' :: List.fromString(in)))
  }
  
  def reCamel(in : String) = {
    def loop(x : List[Char]) : List[Char] = x match {
      case c :: rest if (Character.isUpperCase(c)) => '_' :: Character.toLowerCase(c) :: loop(rest)
      case c :: rest => c :: loop(rest)
      case Nil => Nil
    }
    
    List.toString(Character.toLowerCase(in.charAt(0)) :: loop(List.fromString(in.substring(1))))
  }
  
  val random = new java.security.SecureRandom
  
  def randomString(size: int) : String = {
    var pos = 0
    val sb = new StringBuilder(size)
    var lastRand = 0
    while (pos < size) {
      if ((pos % 6) == 0) lastRand = random.nextInt
      val n = lastRand & 0x1f
      lastRand = lastRand >> 5
      sb.append(if (n < 26) ('A' + n).asInstanceOf[char] else ('0' + (n - 26)).asInstanceOf[char])
      pos = pos + 1
    }
    sb.toString
  }
  
  val hourFormat = new SimpleDateFormat("HH:mm:ss")
  
  def hourFormat(in: Date) = {
    hourFormat.format(in)
  }    
  
  /**
   * Remove all the characters from a string exception a-z, A-Z, 0-9, and '_'
   */ 
  def clean(in : String) =  if (in == null) "" else in.replaceAll("[^a-zA-Z0-9_]", "")
  
  /**
   * Convert a 
   */
  def toBoolean(in: Any): boolean = {
    in match {
      case null => false
      case b : boolean => b
      case i: int => i != 0
      case lo: long => lo != 0
      case n : Number => n.intValue != 0
      case s : String => {
     	var sl = s.toLowerCase
        if (sl.length == 0) false
        else {
          if (sl.charAt(0) == 't') true
          else toInt(s) != 0
        }
      }
      case None => false
      case Some(n) => toBoolean(n)
      case x :: xs => toBoolean(x)
      case o => toBoolean(o.toString)
    }
  }
  
  def toInt(in: Any): int = {
    in match {
      case null => 0
      case n: int => n
      case lo: long => lo.toInt
      case n : Number => n.intValue
      case (n: Number) :: _ => n.intValue
      case Some(n) => toInt(n)
      case None => 0
      case s : String => parseNumber(s)._1.toInt
      case x :: xs => toInt(x)
      case o => toInt(o.toString)
    }
  }
  
  def toLong(in: Any): long = {
    in match {
      case null => 0L
      case i: int => i
      case n: long => n
      case n : Number => n.longValue
      case (n: Number) :: _ => n.longValue
      case Some(n) => toLong(n)
      case None => 0L
      case s : String => parseNumber(s)._1
      case x :: xs => toLong(x)
      case o => toLong(o.toString)
    }
  }
  
  def parseNumber(in: String): (long, int) = {
    if (in == null || in.length == 0) return (0L, 0)
    var num = 0L
    var pos = 0
    val len = in.length
    while (pos < len) {
      val chr = in.charAt(pos)
      if (!java.lang.Character.isDigit(chr)) return (num, pos)
      num = num * 10
      val tn: int = chr - '0'
      num = num + tn
      pos = pos + 1
    }
    return (num, pos)
  }
  
  def toDate(in: String): Date = {
    new Date(in)
  }
  
  def toDate(in: Any): Date = {
    in match {
      case null => null
      case d : Date => d
      case d : java.sql.Date => new Date(d.getTime)
      case lng : java.lang.Long => new Date(lng.longValue)
      case lng : long => new Date(lng)
      case s : String => toDate(s)
      case o => toDate(o.toString)
    }
  }
  
  def printTime[T](msg: String)(f: => T): T = {
    val start = System.currentTimeMillis
    try {
      f
    } finally {
      Console.println(msg+" took "+(System.currentTimeMillis - start)+" Milliseconds")
    }
  }
  
  def calcTime(f: => Any): long = {
    val start = System.currentTimeMillis
    f
    System.currentTimeMillis - start
  }
  
  def createInvoker(name: String, on: AnyRef): Option[() => Option[Any]] = {
    on match {
      case null => None
      case o => {
        o.getClass.getDeclaredMethods.filter{m => m.getName == name && 
					     Modifier.isPublic(m.getModifiers) &&
					     m.getParameterTypes.length == 0}.toList match {
					       case Nil => None
					       case x :: xs => Some(() => {
						 try {
						   Some(x.invoke(o, null))
						 } catch {
						   case e : InvocationTargetException => throw e.getCause
						 }
					       }
								  )
					     }
      }
    }
  }
  
  def base64Encode(in: Array[byte]): String = {
    new String((new Base64).encode(in))
  }
  
  def base64Decode(in: String): Array[byte] = {
    (new Base64).decode(in.getBytes("UTF-8"))
  }
  
  def toByteArrayInputStream(in: InputStream) = {
    val ba = new Array[byte](4096)
    val bos = new ByteArrayOutputStream
    var len = 0
    while (len >= 0) {
      len = in.read(ba)
      if (len > 0) {
        bos.write(ba,0,len)
      }
    }

    new ByteArrayInputStream(bos.toByteArray)
  }
  
  def splitColonPair(in: String, first: String, second: String): (String, String) = {
    (if (in == null) "" else in).split(":").filter{s => s.length > 0}.toList match {
      case f :: s :: _ => (f,s)
	case f :: Nil => (f, second)
	  case _ => (first, second)
    }
  }

  
  def hash(in : String) : String = {
    new String((new Base64) encode (MessageDigest.getInstance("SHA")).digest(in.getBytes("UTF-8")))
  }
  
  def hash(in : Array[byte]) : Array[byte] = {
    (MessageDigest.getInstance("SHA")).digest(in)
  }
  
  def hash256(in : Array[byte]) : Array[byte] = {
    (MessageDigest.getInstance("SHA-256")).digest(in)
  }
  
  def hexDigest(in: Array[byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA")).digest(in)
    hexEncode(binHash)
  }

  
  def hash256(in : String) : String = {
    new String((new Base64) encode (MessageDigest.getInstance("SHA-256")).digest(in.getBytes("UTF-8")))
  }
  
  def hexDigest256(in: Array[byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA-256")).digest(in)
    hexEncode(binHash)
  }

  def hexEncode(in: Array[byte]): String = {
    val sb = new StringBuilder
    val len = in.length
    def addDigit(in: Array[byte], pos: int, len: int, sb: StringBuilder) {
      if (pos < len) {
        val b: int = in(pos)
        val msb = (b & 0xf0) >> 4
        val lsb = (b & 0x0f)
          sb.append((if (msb < 10) ('0' + msb).asInstanceOf[char] else ('a' + (msb - 10)).asInstanceOf[char]))
        sb.append((if (lsb < 10) ('0' + lsb).asInstanceOf[char] else ('a' + (lsb - 10)).asInstanceOf[char]))
        
	addDigit(in, pos + 1, len, sb)
      }
    }
    addDigit(in, 0, len, sb)
    sb.toString
  }
  
  def sendMail(from: String, to: List[String], subject: String,cc: List[String], body: Map[String, Array[byte]]): boolean = {
    false
  }
  
  implicit def nodeSeqToOptionString(in: NodeSeq): Option[String] = if (in.length == 0) None else Some(in.text)
  
  def readWholeStream(in: InputStream): Array[byte] = {
    val bos = new ByteArrayOutputStream
    val ba = new Array[byte](4096)
    
    def readOnce {
      val len = in.read(ba)
      if (len > 0) bos.write(ba, 0, len)
      if (len >= 0) readOnce
    }
    
    readOnce
    
    bos.toByteArray
  }
  
  def readWholeThing(in: Reader): String = {
    val bos = new StringBuilder
    val ba = new Array[char](4096)
    
    def readOnce {
      val len = in.read(ba)
      if (len < 0) return
      if (len > 0) bos.append(ba, 0, len)
      readOnce
    }
    
    readOnce
    
    bos.toString
  }
  
  def timeNow = new java.util.Date
  def time(when: long) = new java.util.Date(when)
  
  def seconds(in: long): long = in * 1000L
  def minutes(in: long): long = seconds(in) * 60L
  def hours(in:long): long = minutes(in) * 60L
  def days(in: long): long = hours( in) * 24L
  def weeks(in: long): long = days(in) * 7L
  
  /**
    * Looks for a named parameter in the XML element and return it if found
    */
  def xmlParam(in: NodeSeq, param: String): Option[String] = {
    val tmp = (in \ ("@"+param))
    if (tmp.length == 0) None else Some(tmp.text)
  }
  
  class TimeSpan(val len: long) {
    def seconds = TimeSpan(Helpers.seconds(len))
    def minutes = TimeSpan(Helpers.minutes(len))
    def hours = TimeSpan(Helpers.hours(len))
    def days = TimeSpan(Helpers.days(len))
    def weeks = TimeSpan(Helpers.weeks(len))
    def later = TimeSpan(len + System.currentTimeMillis)
    def ago = TimeSpan(System.currentTimeMillis - len)
    
    override def equals(cmp: Any) = {
      cmp match {
        case lo: long => lo == this.len
        case i: int => i == this.len
        case ti: TimeSpan => ti.len == this.len
        case _ => false
      }
    }
    
    def +(in: long) = TimeSpan(this.len + in)
    def -(in: long) = TimeSpan(this.len - in)
  }
  
  object TimeSpan {
    def apply(in: long) = new TimeSpan(in)
    implicit def timeSpanToLong(in: TimeSpan): long = in.len
  }
  
  implicit def intToTimeSpan(in: long): TimeSpan = TimeSpan(in)
  implicit def intToTimeSpan(in: int): TimeSpan = TimeSpan(in)
}

