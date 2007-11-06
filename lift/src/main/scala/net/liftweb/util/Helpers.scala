package net.liftweb.util

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.net.{URLDecoder, URLEncoder}
import scala.collection.mutable.{HashSet, ListBuffer}
import scala.xml.{NodeSeq, Elem, Node, Text, Group, UnprefixedAttribute, Null, Unparsed, MetaData, PrefixedAttribute}
import scala.collection.{Map}
import scala.collection.mutable.HashMap
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import java.util.Date
import java.text.SimpleDateFormat
import java.lang.reflect.Modifier
import org.apache.commons.codec.binary.Base64
import java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader, File, FileInputStream, BufferedReader, InputStreamReader}
import java.security.{SecureRandom, MessageDigest}
import scala.actors.Actor
import scala.actors.Actor._
import java.util.regex._
import java.util.{TimeZone, Calendar}
import java.lang.Character._
import javax.crypto._
import javax.crypto.spec._
import net.liftweb.mapper.{Mapper, MappedField}

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
  } openOr new Date(0L)
  
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
  
  def ^ [T](i: T*): List[T] = i.toList
  
  def goodPath_?(path : String): Boolean = {
    if (path == null || path.length == 0 || !path.startsWith("/") || path.indexOf("/.") != -1) false
    else {
      val lastPoint = path.lastIndexOf('.')
      val lastSlash = path.lastIndexOf('/')
      if (lastPoint <= lastSlash) false else {
	validSuffixes.contains(path.substring(lastPoint + 1))
      }
    }
  }
  
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
  
  def couldBeHtml(in: Map[String, String]): Boolean =
    in match {
      case null => true
      case n => {
	n.get("Content-Type") match {
	  case Some(s) => { (s.toLowerCase == "text/html") ||
			    (s.toLowerCase == "application/xhtml+xml") }
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
  
  /**
   * Returns a Full can with the first element x of the list in
   * for which f(x) evaluates to true. If f(x) evaluates to false
   * for every x, then an Empty can is returned.
   *
   * @param in  a list of elements to which f can be applied
   * @param f   a function that can be applied to elements of in
   */
  def first_? [B](in: List[B])(f: => B => Boolean): Can[B] =
    in match {
    case Nil => Empty
    case x :: xs => if (f(x)) Full(x) else first_? (xs)(f)
  }
  
  /**
   * Returns the first application of f to an element of in that
   * results in a Full can. If f applied to an element of in results
   * in an Empty can, then f will be applied to the rest of the
   * elements of in until a Full can results. If the list runs out
   * then an Empty can is returned.
   * 
   * @param in  a list of elements to which f can be applied
   * @param f   a function that can be applied to elements of in
   */  
  def first[B,C](in : List[B])(f : B => Can[C]): Can[C] = {
    in match {
      case Nil => Empty
      case x :: xs => {
	f(x) match {
          case s @ Full(_) =>  s
          case _ => first(xs)(f)
	}
      }
    }
  }
  
  sealed abstract class BindParam {
    def name: String
    // def value: NodeSeq
    def calcValue(in: NodeSeq): NodeSeq
  }
  
case class TheBindParam(name: String, value: NodeSeq) extends BindParam {
  def calcValue(in: NodeSeq): NodeSeq = value
}

case class AttrBindParam(name: String, value: NodeSeq, newAttr: String) extends BindParam {
  def calcValue(in: NodeSeq): NodeSeq = value
}  

case class FuncBindParam(name: String, value: NodeSeq => NodeSeq) extends BindParam {
  def calcValue(in: NodeSeq): NodeSeq = value(in)
}
case class FuncAttrBindParam(name: String, value: NodeSeq => NodeSeq, newAttr: String) extends BindParam {
  def calcValue(in: NodeSeq): NodeSeq = value(in)
}

  implicit def stringThingToBindParam[T](p: (String, T)): BindParam = {
    p._2 match {
      case null => TheBindParam(p._1, Text("null"))
      case s: Symbol => TheBindParam(p._1, Text(s.name))
      case s: String => TheBindParam(p._1, Text(s))
      case n: NodeSeq => TheBindParam(p._1, n)
      case f: (NodeSeq => NodeSeq) => FuncBindParam(p._1, f)
      case Some(s) => stringThingToBindParam((p._1, s))
      case Full(s) => stringThingToBindParam((p._1, s))
      case v => TheBindParam(p._1, Text(p._2.toString))
    }
  }

  def renum[T](in: java.util.Enumeration): List[T] = if (!in.hasMoreElements()) Nil else in.nextElement.asInstanceOf[T] :: renum(in)
  
  implicit def symThingToBindParam[T](p: (Symbol, T)): BindParam = stringThingToBindParam( (p._1.name, p._2))

  /**
   * Experimental template bind. Adopts the approach detailed in my "Template by Example" e-mail to the
   * liftweb discussion list.
   * 
   * @author jorge.ortiz
   */
  def tbind[T<:Mapper[T]](namespace: String, xml: NodeSeq, objs: Seq[T])(transform: T => PartialFunction[String, NodeSeq => NodeSeq]): NodeSeq = {
    val templates = xml.theSeq.headOption.get.child
    
    (for (obj <- objs;
      template <- templates)
        yield xbind(namespace, template)(transform(obj))
    ).flatMap(_.theSeq)
  }
  
  /**
   * Experimental extension to bind which passes in an additional "parameter" from the XHTML to the transform 
   * function, which can be used to format the returned NodeSeq.
   */
  def xbind(namespace: String, xml: NodeSeq)(transform: PartialFunction[String, NodeSeq => NodeSeq]): NodeSeq = {
    def rec_xbind(xml: NodeSeq): NodeSeq = {
      xml.flatMap {
        node => node match {
          case s: Elem if (node.prefix == namespace) =>
            if (transform.isDefinedAt(node.label))
              transform(node.label)(node)
            else
              Text("FIX"+"ME failed to bind <"+namespace+":"+node.label+" />")
          case Group(nodes) => Group(rec_xbind(nodes))
          case s: Elem => Elem(node.prefix, node.label, node.attributes, node.scope, rec_xbind(node.child) : _*)
          case n => node
        }
      }
    }
    
    rec_xbind(xml)
  }
  
  /**
    * Bind a set of values to parameters and attributes in a block of XML 
    */
  def bind(namespace: String, xml: NodeSeq, params: BindParam*): NodeSeq = {
    val map: scala.collection.immutable.Map[String, BindParam] = scala.collection.immutable.HashMap.empty ++ params.map(p => (p.name, p))
    
    def attrBind(attr: MetaData): MetaData = attr match {
      case Null => Null
      case upa: UnprefixedAttribute => new UnprefixedAttribute(upa.key, upa.value, attrBind(upa.next))
      case pa: PrefixedAttribute if pa.pre == namespace => map.get(pa.key) match {
        case None => new PrefixedAttribute(pa.pre, pa.key, Text("FIX"+"ME find to bind attribute"), attrBind(pa.next))
        case Some(abp @ AttrBindParam(_, _, newAttr)) => new UnprefixedAttribute(newAttr, abp.calcValue(pa.value), attrBind(pa.next))
        case Some(abp @ FuncAttrBindParam(_, _, newAttr)) => new UnprefixedAttribute(newAttr, abp.calcValue(pa.value), attrBind(pa.next))
        case Some(bp: TheBindParam) => new PrefixedAttribute(pa.pre, pa.key, bp.calcValue(pa.value), attrBind(pa.next))
        case Some(bp: FuncBindParam) => new PrefixedAttribute(pa.pre, pa.key, bp.calcValue(pa.value), attrBind(pa.next))
      }
      case pa: PrefixedAttribute => new PrefixedAttribute(pa.pre, pa.key, pa.value, attrBind(pa.next))
    }
    
    def in_bind(xml:NodeSeq): NodeSeq = {
      xml.flatMap {
        node =>
          node match {
            case s : Elem if (node.prefix == namespace) => {
              map.get(node.label) match {
		case None => Text("FIX"+"ME failed to bind <"+namespace+":"+node.label+" />")
                case Some(ns) => ns.calcValue(s.child)
              }
            }
	    case Group(nodes) => Group(in_bind(nodes))
            case s : Elem => Elem(node.prefix, node.label, attrBind(node.attributes), node.scope, in_bind(node.child) : _*)
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
  
  def bindlist(listvals: List[Map[String, NodeSeq]], xml: NodeSeq): Can[NodeSeq] = {
    def build (listvals: List[Map[String, NodeSeq]], ret: NodeSeq): NodeSeq = listvals match {
      case Nil => ret
      case vals :: rest => build(rest, ret ++ bind(vals, xml))
    }
    if (listvals.length > 0) Full(build(listvals.drop(1), bind(listvals.head, xml)))
    else Empty
  }

  /**
   * Bind parameters to XML.
   * @param around XML with lift:bind elements
   * @param atWhat data to bind
   */
  def processBind(around: NodeSeq, atWhat: Map[String, NodeSeq]) : NodeSeq = {
    
    /** Find element matched predicate f(x).isDefined, and return f(x) if found or None otherwise. */
    def findMap[A, B](s: Iterable[A])(f: A => Option[B]): Option[B] =
      s.projection.map(f).find(_.isDefined).getOrElse(None)
    
    around.flatMap {
      v =>
        v match {
          case Group(nodes) => Group(processBind(nodes, atWhat))
          case Elem("lift", "bind", attr @ _, _, kids @ _*) =>
            findMap(atWhat) {
	      case (at, what) if attr("name").text == at => Some({what})
	      case _ => None
            }.getOrElse(processBind(v.asInstanceOf[Elem].child, atWhat))
          
          case e: Elem => {Elem(e.prefix, e.label, e.attributes, e.scope, processBind(e.child, atWhat): _*)}
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
        case Full(_) => toInsure
        case _ => field :: toInsure
      }

    headers match {
      case Nil => toInsure
      case x :: xs => insureField(insureField_inner(toInsure, x), xs)
    }
  }
  
  class ListMapish(val theList: List[(String, String)]) {
    def ciGet(swhat: String): Can[String] = {
      val what = swhat.toLowerCase
      def tGet(in: List[(String, String)]): Can[String] = 
	in match {
        case Nil => Empty
        case x :: xs if (x._1.toLowerCase == what) => Full(x._2)
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
  def findClass(name : String, where : List[String]) : Can[Class] =
    findClass(name, where, ^(smartCaps, n => n), s => true)
  
  /**
  * Find a class with name given name in a list of packages, either by matching 'name'
  * or by matching 'smartCaps(name)'
  */
  def findClass(name : String, where : List[String], guard: (Class) => Boolean ) : Can[Class] = {
    findClass(name, where, ^(smartCaps, n => n), guard)
  }
  
  def findClass(where : List[(String, List[String])]) : Can[Class] = {
    where match {
      case Nil => Empty
      case s :: rest => {
	findClass(s._1, s._2) match {
          case Full(s) => Full(s)
          case _ => findClass(rest)
	}
      }
    }
  }

  /**
  * Find a class with name given name in a list of packages, with a list of functions that modify
  * 'name' (e.g., leave it alone, make it camel case, etc.)
  */
  def findClass(name : String, where : List[String], modifiers : List[Function1[String, String]], guard: (Class) => boolean) : Can[Class] = {
    def findClass_s(name : String, where : String) : Can[Class] = {
      tryo(^(classOf[ClassNotFoundException]), Empty) {
        val clzName = where+"."+name
	
        Class.forName(clzName)
      }
    }
    
    
    def findClass_l(name : String, where : List[String]) : Can[Class] = {
      where match {
        case Nil => Empty
        case c :: rest => findClass_s(name, c) or findClass_l(name, rest)
      }
    }
    
    modifiers match {
      case Nil => Empty
      case c :: rest => findClass_l(c(name), where) or findClass(name, where, rest, guard)
    }
  }
  
  /**
  * Wraps a "try" block around the function f.  If f throws
  * an exception with it's class in 'ignore' or of 'ignore' is
  * null or an empty list, ignore the exception and return None.
  */
  def tryo[T](ignore : List[Class],onError: Can[Throwable => Unit])(f : => T) : Can[T] = {
    try {
      Full(f)
    } catch {
      case c if (containsClass(c.getClass, ignore)) => onError.foreach(_(c)); Failure("tryo", Full(c), Nil)
      case c if (ignore == null || ignore.isEmpty) => onError.foreach(_(c)); Failure("tryo", Full(c), Nil)
    }
  }
  
  /**
  * Wraps a "try" block around the function f.  If f throws
  * an exception return None
  */
  def tryo[T](f: => T): Can[T] = tryo(Nil, Empty)(f)
  
  
  /**
  * Wraps a "try" block around the function f.  If f throws
  * an exception return None
  */
  def tryo[T](onError: Throwable => Unit)(f: => T): Can[T] = tryo(Nil, Full(onError))(f)  
  
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
    } openOr false
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
  private def _invokeMethod(clz: Class, meth: String, params: Array[Object], ptypes: Can[Array[Class]]): Can[Any] = {
    /*
    * try to find a method matching the given parameters
    */
    def findMethod : Can[Method] = {
      /* try to find a method with the same name and the same number of arguments. Doesn't check the types.
      * The reason is that it's hard to know for the programmer what is the class name of a given object/class, because scala
      * add some extra $ for ex.
      */
      def findAlternates : Can[Method] = {
        val t = clz.getDeclaredMethods().filter(m=> m.getName.equals(meth)
						&& Modifier.isPublic(m.getModifiers)
						&& m.getParameterTypes.length == params.length)
	if (t.length == 1) Full(t(0))
	else Empty
      }
      try {
        clz.getMethod(meth, ptypes openOr params.map(_.getClass)) match {
            case null => findAlternates
	    case m => Full(m)
          }
      } catch {
        case e: java.lang.NoSuchMethodException => findAlternates
      }
    }
    
    try {
      findMethod.map(m => if (Modifier.isStatic(m.getModifiers)) m.invoke(null, params)
	  else m.invoke(clz.newInstance, params))
    } catch {
      case e: java.lang.IllegalAccessException => Failure("invokeMethod "+meth, Full(e), Nil)
      case e: java.lang.IllegalArgumentException => Failure("invokeMethod "+meth, Full(e), Nil)
    }
  }

  def invokeMethod(clz: Class, meth: String, params: Array[Object]): Can[Any] = {
    _invokeMethod(clz,meth, params, Empty) or _invokeMethod(clz, smartCaps(meth), params, Empty) or
    _invokeMethod(clz, methodCaps(meth), params, Empty)
  }
  
  def invokeMethod(clz: Class, meth: String, params: Array[Object], ptypes: Array[Class]): Can[Any] = {
    _invokeMethod(clz,meth, params, Full(ptypes)) or _invokeMethod(clz, smartCaps(meth), params, Full(ptypes)) or
    _invokeMethod(clz, methodCaps(meth), params, Full(ptypes))
  }

  def invokeMethod(clz: Class, meth: String): Can[Any] = invokeMethod(clz, meth, Nil.toArray)
  
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
  
  def blowfishEncrypt(plain: String, key: Array[Byte]): String = blowfishEncrypt(plain, blowfishKeyFromBytes(key))
  def blowfishEncrypt(plain: String, key: SecretKey): String = base64Encode(blowfishEncrypt(plain.getBytes("UTF-8"), key))
  
  def blowfishDecrypt(enc: String, key: Array[Byte]): String = blowfishDecrypt(enc, blowfishKeyFromBytes(key))
  def blowfishDecrypt(enc: String, key: SecretKey): String = new String(blowfishDecrypt(base64Decode(enc), key), "UTF-8")
  
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
     	val sl = s.toLowerCase
        if (sl.length == 0) false
        else {
          if (sl.charAt(0) == 't') true
          else toInt(s) != 0
        }
      }
      case None => false
      case Empty | Failure(_, _, _) => false
      case Full(n) => toBoolean(n)
      case Some(n) => toBoolean(n)
      case x :: xs => toBoolean(x)
      case o => toBoolean(o.toString)
    }
  }
  
  def toInt(in: Any): Int = {
    in match {
      case null => 0
      case n: Int => n
      case lo: Long => lo.toInt
      case n : Number => n.intValue
      case (n: Number) :: _ => n.intValue
      case Some(n) => toInt(n)
      case Full(n) => toInt(n)
      case None | Empty | Failure(_, _, _) => 0
      case s: String => parseNumber(s).toInt
      case d: java.util.Date => (d.getTime / 1000L).toInt
      case x :: xs => toInt(x)
      case o => toInt(o.toString)
    }
  }
  
  def toLong(in: Any): Long = {
    in match {
      case null => 0L
      case i: Int => i
      case n: Long => n
      case d: java.util.Date => d.getTime
      case n : Number => n.longValue
      case (n: Number) :: _ => n.longValue
      case Some(n) => toLong(n)
      case Full(n) => toLong(n)
      case None | Empty | Failure(_, _, _) => 0L
      case s: String => parseNumber(s)
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
  
  //def toDate(in: String): Date = new Date(in)
  
  def toDate(in: Any): Can[Date] = {
    try {
    in match {
      case null => Empty
      case d: java.util.Date => Full(d)
      case lng: Long => Full(new Date(lng))
      case lng: Number => Full(new Date(lng.longValue))
      case Nil | Empty | None | Failure(_, _, _) => Empty
      case Full(v) => toDate(v)
      case Some(v) => toDate(v)
      case v :: vs => toDate(v)
      case s : String => Full(new Date(s))
      case o => toDate(o.toString)
    }
    } catch {
      case e => Log.debug("Error parsing date "+in, e); Failure("Bad date: "+in, Full(e), Nil)
    }
  }
  
  def currentYear:Int = {
    java.util.Calendar.getInstance.get(java.util.Calendar.YEAR)
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
  
  def createInvoker(name: String, on: AnyRef): Can[() => Can[Any]] = {
    on match {
      case null => Empty
      case o => {
        o.getClass.getDeclaredMethods.filter{
          m => m.getName == name && 
	  Modifier.isPublic(m.getModifiers) &&
	  m.getParameterTypes.length == 0}.toList match {
	    case Nil => Empty
	    case x :: xs => Full(() => {
	      try {
		Full(x.invoke(o, null))
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
  
  def hash(in: String) : String = {
    new String((new Base64) encode (MessageDigest.getInstance("SHA")).digest(in.getBytes("UTF-8")))
  }
  
  def hash(in : Array[Byte]) : Array[byte] = {
    (MessageDigest.getInstance("SHA")).digest(in)
  }
  
  def hash256(in : Array[Byte]) : Array[byte] = {
    (MessageDigest.getInstance("SHA-256")).digest(in)
  }
  
  def hexDigest(in: Array[Byte]): String = {
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
    
  implicit def nodeSeqToOptionString(in: NodeSeq): Can[String] = if (in.length == 0) Empty else Full(in.text)
  
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
  
  /**
    * Given the input date, what's the month (0 based)?
    */
  def month(in: java.util.Date): Int = {
    val cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
    cal.setTimeInMillis(in.getTime)
    cal.get(Calendar.MONTH)
  }
  
  /**
     * Given the input date, what's the year?
     */
   def year(in: java.util.Date): Int =  {
     val cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
     cal.setTimeInMillis(in.getTime)
     cal.get(Calendar.YEAR)
   }
   
  /**
     * Given the input date, what's the day (1 based)?
     */
   def day(in: java.util.Date): Int =  {
     val cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
     cal.setTimeInMillis(in.getTime)
     cal.get(Calendar.DAY_OF_MONTH)
   }
   
  /**
    * The current time as a Date object
    */
  def timeNow = new java.util.Date
  
  /**
    * The current Day as a Date object
    */
  def dayNow: java.util.Date = 0.seconds.later.noTime
  def time(when: long) = new java.util.Date(when)
  
  def seconds(in: long): long = in * 1000L
  def minutes(in: long): long = seconds(in) * 60L
  def hours(in:long): long = minutes(in) * 60L
  def days(in: long): long = hours( in) * 24L
  def weeks(in: long): long = days(in) * 7L
  
  /**
  * Looks for a named parameter in the XML element and return it if found
  */
  def xmlParam(in: NodeSeq, param: String): Can[String] = {
    val tmp = (in \ ("@"+param))
      if (tmp.length == 0) Empty else Full(tmp.text)
  }
  
  class TimeSpan(val len: Long) {
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
    
    def noTime = {
      val div = (12L * 60L * 60L * 1000L)
      val ret = (len - div) / (div * 2L)
      new java.util.Date((ret * (div * 2L)) + div)
    }
    
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
  
  object backgrounder extends Actor {
    def act {
      loop {
        react {
          case BkgExec(f) => f()
        }
      }
    }
  }
  
  backgrounder.start
  
  case class BkgExec(f:() => Any)
  
  def background(f: => Any) {
    backgrounder ! BkgExec(() => f)
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
  
  private def capify(in: String, pos: Int, max: Int, lastLetter: Boolean, lastSymbol: Boolean, out: StringBuilder): Unit = if (pos >= max || pos >= in.length) return
  else {
    in.charAt(pos) match {
      case c if Character.isDigit(c) => out.append(c); capify(in, pos + 1, max, false, false, out)
      case c if Character.isLetter(c) => out.append(if (lastLetter) c else Character.toUpperCase(c)) ; capify(in, pos + 1, max, true, false, out)
      case c if (c == ' ' || c == '_') && !lastSymbol => out.append(c) ; capify(in, pos + 1, max, false, true, out)
      case _ => capify(in, pos + 1, max, false, true, out)
    }
  }
  
  def capify(in: String): String = {
    val tmp = ((in match {
      case null => ""
    case s => s
  }).trim match {
    case "" => "n/a"
    case s => s
  }).toLowerCase
  val sb = new StringBuilder
  capify(tmp, 0, 250, false, false, sb)
  sb.toString
  }  
  
  /*
  private val defaultFinder = getClass.getResource _
  private var _finder = defaultFinder
  
  def setResourceFinder(in: (String) => java.net.URL):unit = synchronized {
    _finder = in
  }
  
  def resourceFinder = synchronized {_finder}
  
  def getResource(name: String): Can[java.net.URL] = resourceFinder(name) match {case null => defaultFinder(name) match {case null => Empty; case s => Full(s)} ; case s => Full(s)} 
  def getResourceAsStream(name: String): Can[java.io.InputStream] = getResource(name).map(_.openStream)
  def loadResource(name: String): Can[Array[Byte]] = getResourceAsStream(name).map{
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
  def loadResourceAsXml(name: String): Can[NodeSeq] = loadResourceAsString(name).flatMap(s =>PCDataXmlParser(s))
  def loadResourceAsString(name: String): Can[String] = loadResource(name).map(s => new String(s, "UTF-8"))
  */
    
  def script(theScript: String): NodeSeq = (<script>
  // {Unparsed("""<![CDATA[
  """+theScript+"""
  // ]]>
  """)}</script>)
  
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
 
 //implicit def optionToDouble[T](in: Option[Option[T]]): DoubleOption[T] = new DoubleOption(in)
 
 implicit def stringToSuper(in: String): SuperString = new SuperString(in)
}

class SuperList[T](val what: List[T]) {
  def headOr(other: => T): T = if (what.isEmpty) other else what.head
  def or(other: => List[T]): List[T] = if (!what.isEmpty) what else other
  def str: String = what.mkString("")
  def comma: String = what.mkString(", ")
  def join(str: String) = what.mkString(str)
  def ? : Boolean = !what.isEmpty
}

class SuperString(val what: String) {
  def roboSplit(spl: String): List[String] = what.split(spl).toList.map(_.trim).filter(_.length > 0)
  def splitAt(chr: String): List[(String, String)] = what.indexOf(chr) match {
    case -1 => Nil
    case n => List((what.substring(0, n).trim, what.substring(n + chr.length).trim))
  }
  
  def encJs: String = {
    val len = what.length
    val sb = new StringBuilder(len * 2)
    sb.append('\'')
    var pos = 0
    while (pos < len) {
      what.charAt(pos) match {
        case c @ ('\\' | '\'') => sb.append(escChar(c))
        case c if c < ' ' || c > '~' => sb.append(escChar(c))
        case c => sb.append(c) 
      }
      pos += 1
    }
    sb.append('\'')
    sb.toString
  }

  
  def escChar(in: Char): String = {
    val ret = Integer.toString(in.toInt, 16)
    "\\u"+("0000".substring(ret.length))+ret
  }
  
  def commafy: String = {
    if (what eq null) null
    else {
    val toDo = what.toList.reverse
    
    def commaIt(in: List[Char]): List[Char] = in match {
      case Nil => in
      case x :: Nil => in
      case x1 :: x2 :: Nil => in
      case x1 :: x2 :: x3 :: Nil => in
      case x1 :: x2 :: x3 :: xs => x1 :: x2 :: x3 :: ',' :: commaIt(xs)
    }
    
    commaIt(toDo).reverse.mkString("")
  }
  }
}

/*
class DoubleOption[T](val what: Option[Option[T]]) {
  def flatten: Option[T] = what.flatMap(a => a)
}*/

// vim: set ts=2 sw=2 et:
