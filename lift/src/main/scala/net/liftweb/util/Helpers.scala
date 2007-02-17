package net.liftweb.util

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import java.net.{URLDecoder, URLEncoder}
import scala.collection.mutable.{HashSet}
import scala.xml.{NodeSeq, Elem}
import scala.collection.{Map}
import scala.collection.mutable.HashMap
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import java.util.Date
import java.text.SimpleDateFormat
import java.lang.reflect.Modifier

/**
  *  A bunch of helper functions
  */
object Helpers {

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
  
  def first[B,C](in : List[B])(f : B => Option[C]) : Option[C] = {
    in match {
      case Nil => None
      case x :: xs => {f(x) match {
         case s @ Some(_) => s
         case None => first(xs)(f)
       }
    }
  }
  }
  
  
  def processBind(around : NodeSeq, at : String, what : NodeSeq) : NodeSeq = {
    around.flatMap {
      v =>
      v match {
      case Elem("mondo", "bind", attr @ _, _, kids @ _*) if (attr("name").text == at) => {what}
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

      if (headers.isEmpty) if (toInsure == null) new HashMap else toInsure
                           else insureField(insureField_inner(toHashMap(toInsure), headers.head), headers.tail) 

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
        case s :: rest => {findClass(s._1, s._2) match {
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
   
     /**
  * Turns a string of format "foo_bar" into camel case "FooBar"
  *
  * Functional code courtesy of Jamie Webb (j@jmawebb.cjb.net) 2006/11/28 
  * @param in The String to CamelCase
  *
  * @return the CamelCased string
  */
  def smartCaps(in : String) = {
    def loop(x : List[Char]) : List[Char] = x match {
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
    List.range(0, size).map{i => val n = random.nextInt(32)
      if (n < 26) ('A' + n).asInstanceOf[char]
      else ('0' + (n - 26)).asInstanceOf[char]
      }.mkString("","","")
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
        case n : Number => n.intValue != 0
        case s : String => {
     	  var sl = s.toLowerCase
          if (sl.length == 0) false
          else {
            if (sl.charAt(0) == 't') true
            else tryn {Integer.parseInt(sl) != 0}
          }
      }
        case o => toBoolean(o.toString)
    }
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
}
