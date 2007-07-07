package net.liftweb.util

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.net.{URLDecoder, URLEncoder}
import scala.collection.mutable.{HashSet}
import scala.xml.{NodeSeq, Elem, Node, Text, Group, UnprefixedAttribute, Null}
import scala.collection.{Map}
import scala.collection.mutable.HashMap
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import java.util.Date
import java.text.SimpleDateFormat
import java.lang.reflect.Modifier
import org.apache.commons.codec.binary.Base64
import java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader, File, FileInputStream}
import java.security.{SecureRandom, MessageDigest}
import scala.actors.Actor
import scala.actors.Actor._
import java.util.regex._
import java.util.TimeZone
import java.lang.Character._
import javax.crypto._
import javax.crypto.spec._

/**
 *  A bunch of helper functions
 */
object Helpers {
  private var otherPackages: List[String] = Nil
  
  val utc = TimeZone.getTimeZone("UTC")
  def internetDateFormatter = {
    val ret = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss z")
    ret.setTimeZone(utc)
    ret
  }
  
  def parseInternetDate(dateString: String): Date = tryo {
    internetDateFormatter.parse(dateString)
  } getOrElse new Date(0L)
  
  def toInternetDate(in: Date): String = internetDateFormatter.format(in)
  def toInternetDate(in: long): String = internetDateFormatter.format(new Date(in))
  
  def buildPackage(end: String)  = synchronized {otherPackages.map{op => op+"."+end}}
  
  def addToPackages(what: String) {synchronized {otherPackages = what :: otherPackages}}
  
  /**
  * URL decode the string.  A pass-through to Java's URL decode with UTF-8
  */
  def urlDecode(in : String) = {URLDecoder.decode(in, "UTF-8")}
  def urlEncode(in : String) = {URLEncoder.encode(in, "UTF-8")}
  
  val validSuffixes = {
    val ret = new HashSet[String];
    ret += ("png", "js", "css", "jpg", "ico", "gif", "tiff", "jpeg")
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
  
  def couldBeHtml(in: Map[String, String]): Boolean =
    in match {
      case null => true
      case n => {
	n.get("Content-Type") match {
	  case Some(s) => s.toLowerCase == "text/html"
	  case None => true
	}
      }
    }
  
  def noHtmlTag(in: NodeSeq): Boolean = (in \\ "html").length != 1
  
  def toHashMap[A,B](in : Map[A,B]) : HashMap[A,B] = {
    val ret = new HashMap[A,B];
    in.keys.foreach {
      k =>
	ret += k -> in(k)
    }
    
    ret
  }
  
  def first_? [B](in: List[B])(f: => B => Boolean): Option[B] =
    in match {
    case Nil => None
    case x :: xs => if (f(x)) Some(x) else first_? (xs)(f)
  }
  
  
  def first[B,C](in : List[B])(f : B => Option[C]): Option[C] = {
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
  
  case class BindParam(name: String, value: NodeSeq)
  
  implicit def stringThingToBindParam[T](p: (String, T)): BindParam = {
    p._2 match {
      case null => BindParam(p._1, Text("null"))
      case s: Symbol => BindParam(p._1, Text(s.name))
      case s: String => BindParam(p._1, Text(s))
      case n: NodeSeq => BindParam(p._1, n)
      case Some(s) => stringThingToBindParam((p._1, s))
      case v => BindParam(p._1, Text(p._2.toString))
    }
  }
  
  def renum[T](in: java.util.Enumeration): List[T] = if (!in.hasMoreElements()) Nil else in.nextElement.asInstanceOf[T] :: renum(in)
  
  implicit def symThingToBindParam[T](p: (Symbol, T)): BindParam = stringThingToBindParam( (p._1.name, p._2))
  
  def bind(namespace: String, xml: NodeSeq, params: BindParam*): NodeSeq = {
    val map: scala.collection.immutable.Map[String, NodeSeq] = scala.collection.immutable.HashMap.empty ++ params.map(p => (p.name, p.value))
    
    def in_bind(xml:NodeSeq): NodeSeq = {
      xml.flatMap {
        node =>
          node match {
            case s : Elem if (node.prefix == namespace) => {
              map.get(node.label) match {
		case None => Text("FIX"+"ME failed to bind <"+namespace+":"+node.label+" />")
            case Some(ns) => ns
              }
            }
	    case Group(nodes) => Group(in_bind(nodes))
            case s : Elem => Elem(node.prefix, node.label, node.attributes,node.scope, in_bind(node.child) : _*)
            case n => node
          }
      }
    }
    in_bind(xml)
  }
  
  def bind(vals: Map[String, NodeSeq], xml: NodeSeq): NodeSeq = {
    xml.flatMap {
      node =>
        node match {
          case s : Elem if (node.prefix == "lift" && node.label == "bind") => {
            node.attributes.get("name") match {
              case None => bind(vals, node.child)
              case Some(ns) => {
                vals.get(ns.text) match {
                  case None => bind(vals, node.child)
                  case Some(nodes) => nodes
                }
              }
            }
          }
	  case Group(nodes) => Group(bind(vals, nodes))
          case s : Elem => Elem(node.prefix, node.label, node.attributes,node.scope, bind(vals, node.child) : _*)
          case n => node
        }
    }
  }
  
  def bindlist(listvals: List[Map[String, NodeSeq]], xml: NodeSeq): Option[NodeSeq] = {
    def build (listvals: List[Map[String, NodeSeq]], ret: NodeSeq): NodeSeq = listvals match {
      case Nil => ret
      case vals :: rest => build(rest, ret ++ bind(vals, xml))
    }
    if (listvals.length > 0) Some(build(listvals.drop(1), bind(listvals.head, xml)))
    else None
  }

  def processBind(around: NodeSeq, at: String, what: NodeSeq) : NodeSeq = {
    around.flatMap {
      v =>
	v match {
	  case Group(nodes) => Group(processBind(nodes, at, what))
	  case Elem("lift", "bind", attr @ _, _, kids @ _*) if (attr("name").text == at) => {what}
          case Elem(_,_,_,_,kids @ _*) => {Elem(v.prefix, v.label, v.attributes, v.scope, processBind(kids, at, what) : _*)}
          case _ => {v}
	}
      
    }
  }
  
  /**
  * Insure all the appropriate fields are in the header
  */
  def insureField(toInsure: List[(String, String)], headers: List[(String, String)]): List[(String, String)] = {
    def insureField_inner(toInsure : List[(String, String)], field : (String, String)): List[(String, String)] =
      toInsure.ciGet(field._1) match {
        case Some(_) => toInsure
        case None => field :: toInsure
      }

    headers match {
      case Nil => toInsure
      case x :: xs => insureField(insureField_inner(toInsure, x), xs)
    }
  }
  
  class ListMapish(val theList: List[(String, String)]) {
    def ciGet(swhat: String): Option[String] = {
      val what = swhat.toLowerCase
      def tGet(in: List[(String, String)]): Option[String] = 
	in match {
        case Nil => None
        case x :: xs if (x._1.toLowerCase == what) => Some(x._2)
        case x :: xs => tGet(xs)
      }
      tGet(theList)
    }
  }
  
  implicit def listToListMapish(in: List[(String, String)]): ListMapish = new ListMapish(in)
  
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
  def enumToStringList(enum : java.util.Enumeration) : List[String] =
    if (enum.hasMoreElements) enum.nextElement.toString :: enumToStringList(enum) else Nil
  
  
  def head[T](l : List[T], deft: => T) = l match {
      case Nil => deft
      case x :: xs => x
    }
  
  /**
  * Find a class with name given name in a list of packages, either by matching 'name'
  * or by matching 'smartCaps(name)'
  */
  def findClass(name : String, where : List[String]) : Option[Class] =
    findClass(name, where, List(smartCaps, n => n), s => true)
  
  /**
  * Find a class with name given name in a list of packages, either by matching 'name'
  * or by matching 'smartCaps(name)'
  */
  def findClass(name : String, where : List[String], guard: (Class) => Boolean ) : Option[Class] = {
    findClass(name, where, List(smartCaps, n => n), guard)
  }
  
  def findClass(where : List[Pair[String, List[String]]]) : Option[Class] = {
    where match {
      case Nil => None
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
    def findClass_s(name : String, where : String) : Option[Class] = {
      tryo(List(classOf[ClassNotFoundException])) {
        val clzName = where+"."+name
	
        Class.forName(clzName)
      }
    }
    
    
    def findClass_l(name : String, where : List[String]) : Option[Class] = {
      where match {
        case Nil => None
        case c :: rest => findClass_s(name, c) orElse findClass_l(name, rest)
      }
    }
    
    modifiers match {
      case Nil => None
      case c :: rest => findClass_l(c(name), where) orElse findClass(name, where, rest, guard)
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
      case c if (containsClass(c.getClass, ignore)) => None
      case c if (ignore == null || ignore.isEmpty) => None
    }
  }
  
  /**
  * Wraps a "try" block around the function f.  If f throws
  * an exception return None
  */
  def tryo[T](f: => T): Option[T] = tryo(Nil)(f)
  
  def callableMethod_?(meth : Method) = {
    meth != null && meth.getParameterTypes.length == 0 && (meth.getModifiers & java.lang.reflect.Modifier.PUBLIC) != 0
  }
  
  /**
  * Is the clz an instance of (assignable from) any of the classes in the list
  * 
  * @param clz the class to test
  * @param toMatch the list of classes to match against
  * 
  * @return true if clz is assignable from of the matching classes
  */
  def containsClass(clz : Class, toMatch : List[Class]) : Boolean = {
    toMatch match {
      case null | Nil => false
      case c :: rest if (c.isAssignableFrom(clz)) => true
      case c :: rest => containsClass(clz, rest)
    }
  }
  
  def classHasControllerMethod(clz : Class, methName : String): Boolean = {
    tryo {
      clz match {
        case null => false
        case _ => callableMethod_?(clz.getMethod(methName, null))
      }
    } getOrElse false
  }

  def invokeControllerMethod(clz : Class, meth : String) = {
    try {
      clz.getMethod(meth, null).invoke(clz.newInstance, null)
    } catch {
      case c : InvocationTargetException => {def findRoot(e : Throwable) {if (e.getCause == null || e.getCause == e) throw e else findRoot(e.getCause)}; findRoot(c)}
    }
  }
  
  /**
  * Invoke the given method for the given class, with the given params.
  * The class is not instanciated if the method is static, otherwise, a new instance of clz is created.
  */
  private def _invokeMethod(clz: Class, meth: String, params: Array[Object], ptypes: Option[Array[Class]]): Option[Any] = {
    /*
    * try to find a method matching the given parameters
    */
    def findMethod : Option[Method] = {
      /* try to find a method with the same name and the same number of arguments. Doesn't check the types.
      * The reason is that it's hard to know for the programmer what is the class name of a given object/class, because scala
      * add some extra $ for ex.
      */
      def findAlternates : Option[Method] = {
        val t = clz.getDeclaredMethods().filter(m=> m.getName.equals(meth)
						&& Modifier.isPublic(m.getModifiers)
						&& m.getParameterTypes.length == params.length)
	if (t.length == 1) Some(t(0))
	else None
      }
      try {
        clz.getMethod(meth, ptypes match {
          case None => params.map(_.getClass)
	  case Some(a) => a }) match {
            case null => findAlternates
	    case m => Some(m)
          }
      } catch {
        case e: java.lang.NoSuchMethodException => findAlternates
      }
    }
    
    try {
      findMethod match {
        case None => None
        case Some(m) => {
	  if (Modifier.isStatic(m.getModifiers)) Some(m.invoke(null, params))
	  else Some(m.invoke(clz.newInstance, params))
	}
      }
    } catch {
      case e: java.lang.IllegalAccessException => None
      case e: java.lang.IllegalArgumentException => None
    }
  }

  def invokeMethod(clz: Class, meth: String, params: Array[Object]): Option[Any] = {
    _invokeMethod(clz,meth, params, None) orElse _invokeMethod(clz, smartCaps(meth), params, None) orElse
    _invokeMethod(clz, methodCaps(meth), params, None) orElse None
  }
  
  def invokeMethod(clz: Class, meth: String, params: Array[Object], ptypes: Array[Class]): Option[Any] = {
    _invokeMethod(clz,meth, params, Some(ptypes)) orElse _invokeMethod(clz, smartCaps(meth), params, Some(ptypes)) orElse
    _invokeMethod(clz, methodCaps(meth), params, Some(ptypes)) orElse None
  }

  def invokeMethod(clz: Class, meth: String): Option[Any] = invokeMethod(clz, meth, Array())
  
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
  
  private val random = new java.security.SecureRandom
  
  def randomLong(mod: Long): Long = Math.abs(random.nextLong) % mod
  def randomInt(mod: Int): Int = Math.abs(random.nextInt) % mod
  
  def shouldShow(percent: Int): Boolean = Math.abs(random.nextInt) % 100 < percent
  def shouldShow(percent: Double): Boolean = random.nextDouble <= percent
  
  def makeBlowfishKey: Array[Byte] = KeyGenerator.getInstance("blowfish").generateKey.getEncoded
  def blowfishKeyFromBytes(key: Array[Byte]): SecretKey = new SecretKeySpec(key, "blowfish")
  
  def blowfishDecrypt(enc: Array[Byte], key: Array[Byte]): Array[Byte] = blowfishDecrypt(enc, blowfishKeyFromBytes(key))
  def blowfishDecrypt(enc: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(decryptStream(new ByteArrayInputStream(enc), key))
  
  def blowfishEncrypt(plain: Array[Byte], key: Array[Byte]): Array[Byte] = blowfishEncrypt(plain, blowfishKeyFromBytes(key))
  def blowfishEncrypt(plain: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(encryptStream(new ByteArrayInputStream(plain), key))
  
  
  def decryptStream(in: InputStream, key: Array[Byte]): InputStream = decryptStream(in, blowfishKeyFromBytes(key))
  def decryptStream(in: InputStream, key: SecretKey): InputStream = {
      val cipher = Cipher.getInstance("blowfish")
      cipher.init(Cipher.DECRYPT_MODE, key)
      new CipherInputStream(in, cipher)
  }
  
  def encryptStream(in: InputStream, key: Array[Byte]): InputStream= encryptStream(in, blowfishKeyFromBytes(key))
  def encryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("blowfish")
    cipher.init(Cipher.ENCRYPT_MODE, key)
    new CipherInputStream(in, cipher)
  }
  
  def randomString(size: Int): String = {
    def addChar(pos: Int, lastRand: Int, sb: StringBuilder): StringBuilder = {
      if (pos >= size) sb 
      else {
	val randNum = if ((pos % 6) == 0) random.nextInt else lastRand
	sb.append((randNum & 0x1f) match {
	  case n if n < 26 => ('A' + n).toChar
	  case n => ('0' + (n - 26)).toChar
	})
	addChar(pos + 1, randNum >> 5, sb)
      }
    }
    addChar(0, 0, new StringBuilder(size)).toString
  }
  
  val hourFormat = new SimpleDateFormat("HH:mm:ss")
  
  def hourFormat(in: Date): String = {
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
  
  def toInt(in: Any): Int = {
    in match {
      case null => 0
      case n: int => n
      case lo: long => lo.toInt
      case n : Number => n.intValue
      case (n: Number) :: _ => n.intValue
      case Some(n) => toInt(n)
      case None => 0
      case s : String => parseNumber(s).toInt
      case x :: xs => toInt(x)
      case o => toInt(o.toString)
    }
  }
  
  def toLong(in: Any): Long = {
    in match {
      case null => 0L
      case i: int => i
      case n: long => n
      case n : Number => n.longValue
      case (n: Number) :: _ => n.longValue
      case Some(n) => toLong(n)
      case None => 0L
      case s : String => parseNumber(s)
      case x :: xs => toLong(x)
      case o => toLong(o.toString)
    }
  }
  
  def parseNumber(tin: String): Long = {
    def cToL(in: Char) = in.toLong - '0'.toLong
    def p(in: List[Char]) = in.takeWhile(isDigit).foldLeft(0L)((acc,c) => (acc * 10L) + cToL(c))

    if (tin eq null) 0L
    else {
      tin.trim.toList match {
        case '-' :: xs => -p(xs)
        case '+' :: xs => p(xs)
        case xs => p(xs)
      }
    }
  }
  
  def toDate(in: String): Date = new Date(in)
  
  def toDate(in: Any): Date = {
    in match {
      case null => null
      case d : java.util.Date => d
      case lng : java.lang.Long => new Date(lng.longValue)
      case lng : long => new Date(lng)
      case s : String => toDate(s)
      case o => toDate(o.toString)
    }
  }
  
  def millis = System.currentTimeMillis
  
  def logTime[T](msg: String)(f: => T): T = {
    val (time, ret) = calcTime(f)
    Log.info(msg+" took "+time+" Milliseconds")
    ret
    /*
    val start = millis
    try {
      f
    } finally {
      Log.info(msg+" took "+(millis - start)+" Milliseconds")
    }*/
  }
  
  def calcTime[T](f: => T): (Long, T) = {
    val start = millis
    val ret = f
    (millis - start, ret)
  }
  
  def createInvoker(name: String, on: AnyRef): Option[() => Option[Any]] = {
    on match {
      case null => None
      case o => {
        o.getClass.getDeclaredMethods.filter{
          m => m.getName == name && 
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
  
  def base64Encode(in: Array[Byte]): String = {
    new String((new Base64).encode(in))
  }
  
  def base64Decode(in: String): Array[Byte] = {
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

  
  def md5(in: Array[Byte]) = (MessageDigest.getInstance("MD5")).digest(in)
  
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
    
  implicit def nodeSeqToOptionString(in: NodeSeq): Option[String] = if (in.length == 0) None else Some(in.text)
  
  def readWholeFile(file: File): Array[Byte] = readWholeStream(new FileInputStream(file))
  
  def readWholeStream(in: InputStream): Array[Byte] = {
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
  
  def notEq(a: Array[Byte], b: Array[Byte]) = !isEq(a,b)
  def isEq(a: Array[Byte], b: Array[Byte]) = {
    def eq(a: Array[Byte], b: Array[Byte], pos: Int, len: Int): Boolean = {
      if (pos == len) true
      else if (a(pos) != b(pos)) false
      else eq(a,b, pos + 1, len)
    }
    
    val len = a.length
    len == b.length && eq(a,b,0,len)
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
    def second = seconds
    def minutes = TimeSpan(Helpers.minutes(len))
    def minute = minutes
    def hours = TimeSpan(Helpers.hours(len))
    def hour = hours
    def days = TimeSpan(Helpers.days(len))
    def day = days
    def weeks = TimeSpan(Helpers.weeks(len))
    def week = weeks
    def later = TimeSpan(len + millis)
    def ago = TimeSpan(millis - len)
    def date = new java.util.Date(len)
    
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
    override def toString = {
      def moof(in: long, scales: List[(long, String)]): List[long] = {
        val hd = scales.head
        val sc = hd._1
        if (sc == -1L) List(in)
        else (in % sc) :: moof(in / sc, scales.tail)
      }
      
      val lst = moof(len, TimeSpan.scales).zip(TimeSpan.scales.map(_._2)).reverse.dropWhile(_._1 == 0L).map(t => ""+t._1+" "+t._2+(if (t._1 != 1L) "s" else ""))
      lst.mkString("",", ", "")+" ("+len+")"
    }
  }
  
  object TimeSpan {
    def apply(in: long) = new TimeSpan(in)
    implicit def timeSpanToLong(in: TimeSpan): long = in.len
    
    val scales = List((1000L, "ms"), (60L, "second"), (60L, "minute"), (24L, "hour"), (7L, "day"), (-1L, "week"))
  }
  
  implicit def intToTimeSpan(in: long): TimeSpan = TimeSpan(in)
  implicit def intToTimeSpan(in: int): TimeSpan = TimeSpan(in)
  implicit def timeSpanToDate(in: TimeSpan): java.util.Date = new java.util.Date(in.len)
  
  def processString(msg: String, subst: Map[String, String]): String = {
    val pat = Pattern.compile("\\<\\%\\=([^\\%]*)\\%\\>")
    val m = pat.matcher(msg)
    val ret = new StringBuffer
    while (m.find) {
      m.appendReplacement(ret, subst(m.group(1).trim))
    }
    m.appendTail(ret)
    ret.toString
  }
  
  private val defaultFinder = getClass.getResource _
  private var _finder = defaultFinder
  
  def setResourceFinder(in: (String) => java.net.URL):unit = synchronized {
    _finder = in
  }
  
  def resourceFinder = synchronized {_finder}
  
  def getResource(name: String): Option[java.net.URL] = resourceFinder(name) match {case null => defaultFinder(name) match {case null => None; case s => Some(s)} ; case s => Some(s)} 
  def getResourceAsStream(name: String): Option[java.io.InputStream] = getResource(name).map(_.openStream)
  def loadResource(name: String): Option[Array[byte]] = getResourceAsStream(name).map{
    stream =>
      val buffer = new Array[byte](2048)
    val out = new ByteArrayOutputStream
    def reader {
      val len = stream.read(buffer)
      if (len < 0) return
      else if (len > 0) out.write(buffer, 0, len)
      reader
    }
    reader
    stream.close
    out.toByteArray
  }
  def loadResourceAsXml(name: String): Option[NodeSeq] = loadResourceAsString(name).flatMap(s =>PCDataXmlParser(s))
  def loadResourceAsString(name: String): Option[String] = loadResource(name).map(s => new String(s, "UTF-8"))
  
  /**
   * Optional cons that implements the expression: expr ?> value ::: List
   */
  class OptiCons(expr: Boolean) {
    def ?>[T](f: => T): List[T] = if (expr) List(f) else Nil
  }
  
  implicit def toOptiCons(expr: Boolean): OptiCons = new OptiCons(expr)
  
  implicit def toSuperList[T](in: List[T]): SuperList[T] = new SuperList(in)
  
  def listIf[T](expr: Boolean)(f: => T): List[T] = if(expr) List(f) else Nil
  
  abstract class MyOption[+A] {
    def |[B >: A](default: => B): B
  }
  case class MySome[+A](x: A) extends MyOption[A] {
    def |[B >: A](default: => B): B  = x
  }
  case class MyNone extends MyOption[Nothing] {
    def |[B ](default: => B): B  = default
  }

  class Boolean2(b: Boolean) {
    def ? [A](first: A): MyOption[A] = {
        if (b) MySome(first)
        else MyNone
    }
  }

 implicit def boolean2(b: Boolean) = new Boolean2(b) 
 
 implicit def pairToUnprefixed(in: (String, Any)): UnprefixedAttribute = new UnprefixedAttribute(in._1, in._2.toString, Null)
}

class SuperList[T](val what: List[T]) {
  def headOr(other: => T): T = if (what.isEmpty) other else what.head
  def or(other: => List[T]): List[T] = if (!what.isEmpty) what else other
  def str: String = what.mkString("")
  def comma: String = what.mkString(", ")
  def join(str: String) = what.mkString(str)
  def ? : Boolean = !what.isEmpty
}
