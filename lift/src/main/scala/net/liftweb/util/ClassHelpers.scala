package net.liftweb.util
import java.lang.reflect.{Method, Modifier, InvocationTargetException}

/**
 * ClassHelpers provide several functions to instantiate a Class object given the class name and one or more package names
 */
trait ClassHelpers { self: ControlHelpers =>
  
  private val nameModifiers = List[String => String](smartCaps _, n => n)
  
  /**
   * utility function returning varargs as a List
   * @return the list of arguments passed as varargs
   */
  def ^ [T](i: T*): List[T] = i.toList

  /**
   * General method to in find a class according to its name, a list of possible packages and a list of functions modifying the given name 
   * create a target name to look for (e.g: 'name' is hello_world and the target name may be 'HelloWorld').
   * @parameter name name of the class to find
   * @parameter where list of package names which may contain the class
   * @parameter modifiers list of functions that modify the 'name' of the class (e.g., leave it alone, make it camel case, etc.)
   * @parameter targetType optional expected type which the retrieved class should conform to
   *
   * @return a Can, either containing the found class or an Empty can.
   */
  def findClass[C <: AnyRef](name: String, where: List[String], modifiers: List[Function1[String, String]], targetType: Can[Class[C]]): Can[Class[C]] = {
    
    // Find the class in a single package. Return a can containing the found class or Empty
    def findClass_s(name : String, where: String): Can[Class[C]] = {
      tryo(classOf[ClassNotFoundException]) {
        val c = Class.forName(where + "." + name).asInstanceOf[Class[C]]
        targetType match {
          case Empty => c 
          case Full(t) => tryo(classOf[ClassCastException]) { c.asSubclass(t); c } openOr(throw new ClassNotFoundException) 
        }
      }
    }
    
    // Find the class in a list of packages. Return a can containing the found class or Empty
    def findClass_l(name: String, where: List[String]): Can[Class[C]] = {
      where match {
        case Nil => Empty
        case c :: rest => findClass_s(name, c) or findClass_l(name, rest) 
      }
    }
    
    modifiers match {
      case Nil => Empty
      case tryName :: rest => findClass_l(tryName(name), where) or findClass[C](name, where, rest, targetType)
    }
  }
 
  /**
   * Find a class given its name and a list of packages, turning underscored names to CamelCase if necessary.
   * It doesn't check that the resulting class isAssignable by Class[C]
   * @parameter name name of the class to find
   * @parameter where list of package names which may contain the class
   * @parameter modifiers list of functions that modify the 'name' of the class (e.g., leave it alone, make it camel case, etc.)
   *
   * @return a Can, either containing the found class or an Empty can.
   */
  def findClass[C <: AnyRef](name: String, where: List[String], modifiers: List[Function1[String, String]]): Can[Class[C]] = findClass(name, where, modifiers, Empty)

  /**
   * Find a class given its name and a list of packages, turning underscored names to CamelCase if necessary.
   * @parameter name name of the class to find
   * @parameter where list of package names which may contain the class
   * @parameter targetType optional expected type which the retrieved class should conform to
   *
   * @return a Can, either containing the found class or an Empty can.
   */
  def findClass[C <: AnyRef](name: String, where: List[String], targetType: Can[Class[C]]): Can[Class[C]] = findClass(name, where, nameModifiers, targetType)

  /**
   * Find a class given its name and a list of packages, turning underscored names to CamelCase if necessary 
   * @parameter name name of the class to find
   * @parameter where list of package names which may contain the class
   *
   * @return a Can, either containing the found class or an Empty can.
   */
  def findClass[C <: AnyRef](name: String, where: List[String]): Can[Class[C]] =
    findClass[C](name, where, nameModifiers)
  
  /**
   * Find a class given a list of possible names and corresponding packages, turning underscored names to CamelCase if necessary 
   * @parameter where list of pairs (name, package names) which may contain the class
   *
   * @return a Can, either containing the found class or an Empty can.
   */
  def findClass[C <: AnyRef](where: List[(String, List[String])]): Can[Class[C]] = where match {
    case Nil => Empty
    case s :: rest => {
      findClass[C](s._1, s._2) match {
        case Full(s) => Full(s)
        case _ => findClass[C](rest)
      }
    }
  }
  
  /**
   * Turns a string of format "foo_bar" into camel case "FooBar"
   *
   * Functional code courtesy of Jamie Webb (j@jmawebb.cjb.net) 2006/11/28 
   * @param name the String to CamelCase
   *
   * @return the CamelCased string
   */
  def smartCaps(name : String) = {
    def loop(x : List[Char]) : List[Char] = (x: @unchecked) match {
      case '_' :: '_' :: rest => loop('_' :: rest)
      case '_' :: c :: rest => Character.toUpperCase(c) :: loop(rest)
      case c :: rest => c :: loop(rest)
      case Nil => Nil
    }
    
    List.toString(loop('_' :: List.fromString(name)))
  }

  
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
  def containsClass[C, CL](clz: Class[C], toMatch : List[Class[CL]]) : Boolean = {
    toMatch match {
      case null | Nil => false
      case c :: rest if (c.isAssignableFrom(clz)) => true
      case c :: rest => containsClass(clz, rest)
    }
  }
  
  def classHasControllerMethod(clz: Class[_], methName: String): Boolean = {
    tryo {
      clz match {
        case null => false
        case _ => callableMethod_?(clz.getMethod(methName, null))
      }
    } openOr false
  }
  
  def invokeControllerMethod(clz: Class[_], meth: String) = {
    try {
      clz.getMethod(meth, null).invoke(clz.newInstance, null)
    } catch {
      case c : InvocationTargetException => {def findRoot(e : Throwable) {if (e.getCause == null || e.getCause == e) throw e else findRoot(e.getCause)}; findRoot(c)}
    }
  }
  
  type Garb = T forSome {type T}
  
  /**
  * Invoke the given method for the given class, with the given params.
  * The class is not instanciated if the method is static, otherwise, a new instance of clz is created.
  */
  private def _invokeMethod[C](clz: Class[C], inst: AnyRef, meth: String, params: Array[AnyRef], ptypes: Can[Array[(Class[CL] forSome {type CL})]]): Can[Any] = {
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
        // openOr params.map(_.getClass).asInstanceOf[Array[Class[AnyRef]]]
        clz.getMethod(meth, ptypes openOr params.map(_.getClass) ) match {
          case null => findAlternates
          case m => Full(m)
        }
      } catch {
        case e: java.lang.NoSuchMethodException => findAlternates
      }
    }
    
    try {
      findMethod.map(m => if (Modifier.isStatic(m.getModifiers)) m.invoke(null, params)
      else m.invoke(inst, params))
    } catch {
      case e: java.lang.IllegalAccessException => Failure("invokeMethod "+meth, Full(e), Nil)
      case e: java.lang.IllegalArgumentException => Failure("invokeMethod "+meth, Full(e), Nil)
    }
  }
  
  def instantiate[C](clz: Class[C]): Can[C] = tryo{clz.newInstance}
  
  def invokeMethod[C](clz: Class[C], inst: AnyRef, meth: String, params: Array[Object]): Can[Any] = {
    _invokeMethod(clz, inst, meth, params, Empty) or _invokeMethod(clz, inst, smartCaps(meth), params, Empty) or
    _invokeMethod(clz, inst, methodCaps(meth), params, Empty)
  }
  
  // def runMethod(inst: AnyRef, meth: String, params: Array[AnyRef]): Can[Any] = Empty
  
  // def runMethod(inst: AnyRef, meth: String): Can[Any] = runMethod(inst, meth, Array())
  
  def invokeMethod[C](clz: Class[C], inst: AnyRef, meth: String, params: Array[AnyRef], ptypes: Array[(Class[CL] forSome {type CL})]): Can[Any] = {
    _invokeMethod(clz, inst, meth, params, Full(ptypes)) or
    _invokeMethod(clz, inst, smartCaps(meth), params, Full(ptypes)) or
    _invokeMethod(clz, inst, methodCaps(meth), params, Full(ptypes))
  }
  
  def invokeMethod[C](clz: Class[C], inst: AnyRef, meth: String): Can[Any] = invokeMethod(clz, inst, meth, Nil.toArray)
  
  def methodCaps(name: String): String = {
    val tmp = smartCaps(name)
    tmp.substring(0,1).toLowerCase + tmp.substring(1)
  }
  
  def reCamel(in : String) = {
    def loop(x : List[Char]) : List[Char] = x match {
      case c :: rest if (Character.isUpperCase(c)) => '_' :: Character.toLowerCase(c) :: loop(rest)
      case c :: rest => c :: loop(rest)
      case Nil => Nil
    }
    
    List.toString(Character.toLowerCase(in.charAt(0)) :: loop(List.fromString(in.substring(1))))
  }
  
  def createInvoker[C <: AnyRef](name: String, on: C): Can[() => Can[Any]] = {
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
}
