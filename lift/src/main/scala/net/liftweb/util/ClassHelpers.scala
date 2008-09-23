package net.liftweb.util
import _root_.java.lang.reflect.{Method, Modifier, InvocationTargetException}
import _root_.java.lang.reflect.Modifier._

/**
 * ClassHelpers provide several functions to instantiate a Class object given the class name and one or more package names
 */
trait ClassHelpers { self: ControlHelpers =>

  private val nameModifiers = List[String => String](camelCase _, n => n)

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
  def findClass[C <: AnyRef](name: String, where: List[String], modifiers: List[Function1[String, String]], targetType: Class[C]): Can[Class[C]] = {

    // Find the class in a single package. Return a can containing the found class or Empty
    def findClass_s(name : String, where: String): Can[Class[C]] = {
      tryo(classOf[ClassNotFoundException]) {
        val c = Class.forName(where + "." + name).asInstanceOf[Class[C]]
        tryo(classOf[ClassCastException]) { c.asSubclass(targetType); c } openOr(throw new ClassNotFoundException)
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
  def findClass(name: String, where: List[String], modifiers: List[Function1[String, String]]): Can[Class[AnyRef]] = findClass(name, where, modifiers, classOf[AnyRef])

  /**
   * Find a class given its name and a list of packages, turning underscored names to CamelCase if necessary.
   * @parameter name name of the class to find
   * @parameter where list of package names which may contain the class
   * @parameter targetType optional expected type which the retrieved class should conform to
   *
   * @return a Can, either containing the found class or an Empty can.
   */
  def findClass[C <: AnyRef](name: String, where: List[String], targetType: Class[C]): Can[Class[C]] = findClass(name, where, nameModifiers, targetType)

  /**
   * Find a class given its name and a list of packages, turning underscored names to CamelCase if necessary
   * @parameter name name of the class to find
   * @parameter where list of package names which may contain the class
   *
   * @return a Can, either containing the found class or an Empty can.
   */
  def findClass(name: String, where: List[String]): Can[Class[AnyRef]] =
    findClass(name, where, nameModifiers)

  /**
   * Find a class given a list of possible names and corresponding packages, turning underscored names to CamelCase if necessary
   * @parameter where list of pairs (name, package names) which may contain the class
   *
   * @return a Can, either containing the found class or an Empty can.
   */
  def findClass(where: List[(String, List[String])]): Can[Class[AnyRef]] = where match {
    case Nil => Empty
    case s :: rest => {
      findClass(s._1, s._2) match {
        case Full(s) => Full(s)
        case _ => findClass(rest)
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
  def camelCase(name : String): String = {
    def loop(x : List[Char]): List[Char] = (x: @unchecked) match {
      case '_' :: '_' :: rest => loop('_' :: rest)
      case '_' :: c :: rest => Character.toUpperCase(c) :: loop(rest)
      case '_' :: Nil => Nil
      case c :: rest => c :: loop(rest)
      case Nil => Nil
    }
    if (name == null)
      ""
    else
      List.toString(loop('_' :: name.toList))
  }

  /**
   * Turn a string of format "foo_bar" into camel case with the first letter in lower case: "fooBar"
   * This function is especially used to camelCase method names.
   *
   * @param name the String to CamelCase
   *
   * @return the CamelCased string
   */
  def camelCaseMethod(name: String): String = {
    val tmp: String = camelCase(name)
    if (tmp.length == 0)
      ""
    else
      tmp.substring(0,1).toLowerCase + tmp.substring(1)
  }

  /**
   * Turn a string of format "FooBar" into camel case "foo_bar"
   *
   * @return the underscored string
   */
  def unCamelCase(name : String) = {
    def loop(x : List[Char]) : List[Char] = x match {
      case c :: rest if (Character.isUpperCase(c)) => '_' :: Character.toLowerCase(c) :: loop(rest)
      case c :: rest => c :: loop(rest)
      case Nil => Nil
    }
    if (name.isEmpty)
      ""
    else
      List.toString(Character.toLowerCase(name.charAt(0)) :: loop(name.substring(1).toList))
  }

  /**
   * @return true if the method is public and has no parameters
   */
  def callableMethod_?(meth: Method) = {
    meth != null && meth.getParameterTypes.length == 0 && isPublic(meth.getModifiers)
  }

  /**
   * Is the clz an instance of (assignable from) any of the classes in the list
   *
   * @param clz the class to test
   * @param toMatch the list of classes to match against
   *
   * @return true if clz is assignable from any of the matching classes
   */
  def containsClass[C](clz: Class[C], toMatch: List[Class[CL] forSome {type CL}]): Boolean = {
    toMatch match {
      case null | Nil => false
      case c :: rest if (c.isAssignableFrom(clz)) => true
      case c :: rest => containsClass(clz, rest)
    }
  }

  /**
   * Check that the method 'name' is callable for class 'clz'
   *
   * @param clz the class supposed to own the method
   * @param name name of the method to test
   *
   * @return true if the method exists on the class and is callable
   */
  def classHasControllerMethod(clz: Class[_], name: String): Boolean = {
    tryo {
      clz match {
        case null => false
        case _ => callableMethod_?(clz.getDeclaredMethod(name))
      }
    } openOr false
  }

  /**
   * Invoke a controller method (parameterless, public) on a class
   *
   * @param clz the class owning the method
   * @param name name of the method to invoke
   *
   * @return the result of the method invocation or throws the root exception causing an error
   */
  def invokeControllerMethod(clz: Class[_], meth: String) = {
    try {
      clz.getMethod(meth).invoke(clz.newInstance)
    } catch {
      case c : InvocationTargetException => {
        def findRoot(e : Throwable) { if (e.getCause == null || e.getCause == e) throw e else findRoot(e.getCause) }
        findRoot(c)
      }
    }
  }

  /**
   * Invoke the given method for the given class, with no params.
   * The class is not instanciated if the method is static, otherwise the passed instance is used
   *
   * @param clz class whose method should be invoked
   * @param inst instance of the class who method should be invoked, if the method is not static
   * @param meth method to invoke
   *
   * @return a Can containing the value returned by the method
   */
  def invokeMethod[C](clz: Class[C], inst: AnyRef, meth: String): Can[Any] = invokeMethod(clz, inst, meth, Nil.toArray)

  /**
   * Invoke the given method for the given class, with some parameters.
   * Tries the method name, then the method as a CamelCased name and the method as a camelCased name
   * The class is not instanciated if the method is static, otherwise the passed instance is used
   *
   * @param clz class whose method should be invoked
   * @param inst instance of the class who method should be invoked, if the method is not static
   * @param meth method to invoke
   * @param params parameters to pass to the method
   *
   * @return a Can containing the value returned by the method
   */
  def invokeMethod[C](clz: Class[C], inst: AnyRef, meth: String, params: Array[AnyRef]): Can[Any] = {
    _invokeMethod(clz, inst, meth, params, Empty) or
    _invokeMethod(clz, inst, camelCase(meth), params, Empty) or
    _invokeMethod(clz, inst, camelCaseMethod(meth), params, Empty)
  }

  /**
   * Invoke the given method for the given class, with some parameters and their types
   * Tries the method name, then the method as a CamelCased name and the method as a camelCased name
   * The class is not instanciated if the method is static, otherwise the passed instance is used
   *
   * @param clz class whose method should be invoked
   * @param inst instance of the class who method should be invoked, if the method is not static
   * @param meth method to invoke
   * @param params parameters to pass to the method
   * @param ptypes list of types of the parameters
   *
   * @return a Can containing the value returned by the method
   */
  def invokeMethod[C](clz: Class[C], inst: AnyRef, meth: String, params: Array[AnyRef], ptypes: Array[(Class[CL] forSome {type CL})]): Can[Any] = {
    _invokeMethod(clz, inst, meth, params, Full(ptypes)) or
    _invokeMethod(clz, inst, camelCase(meth), params, Full(ptypes)) or
    _invokeMethod(clz, inst, camelCaseMethod(meth), params, Full(ptypes))
  }


  /**
   * Invoke the given method for the given class, with the given params.
   * The class is not instanciated if the method is static, otherwise the passed instance is used
   *
   * @param clz class whose method should be invoked
   * @param inst instance of the class who method should be invoked, if the method is not static
   * @param meth method to invoke
   * @param params parameters to pass to the method
   * @param ptypes list of types of the parameters
   *
   * @return a Can containing the value returned by the method
   */
  private def _invokeMethod[C](clz: Class[C], inst: AnyRef, meth: String, params: Array[AnyRef], ptypes: Can[Array[(Class[CL] forSome {type CL})]]): Can[Any] = {
     // try to find a method matching the given parameters
    def possibleMethods: List[Method] = {
      /*
       * try to find a method with the same name and the same number of arguments. Doesn't check the types.
       * The reason is that it's hard to know for the programmer what is the class name of a given object/class, because scala
       * add some extra $ for ex.
       */
      def alternateMethods: List[Method] = clz.getDeclaredMethods.toList.filter( m => m.getName.equals(meth) &&
                                            isPublic(m.getModifiers) &&
                                            m.getParameterTypes.length == params.length)

      try {
        val classes: Array[(Class[CL] forSome {type CL})] = ptypes openOr params.map(_.getClass)
        List(clz.getMethod(meth, classes : _*))
      } catch {
        case e: NullPointerException => Nil
        case e: NoSuchMethodException => alternateMethods
      }
    }
    def findFirst[T, U](l: List[T], f: T => U, predicate: U => Boolean): Can[U] = {
      l match {
        case Nil => Empty
        case x :: xs => {
         val result = f(x)
         if (predicate(result)) Full(result) else findFirst(xs, f, predicate)
        }
      }
    }
    possibleMethods.elements.filter(m => inst != null || isStatic(m.getModifiers)).
                                   map((m: Method) => tryo{m.invoke(inst, params : _*)}).
                                   find((x: Can[Any]) => x match {
                                            case result@Full(_) => true
                                            case Failure(_, Full(c: IllegalAccessException), _) => false
                                            case Failure(_, Full(c: IllegalArgumentException), _) => false
                                            case Failure(_, Full(c), _) => if (c.getCause != null) throw c.getCause else throw c
                                   }) match {
                                            case Some(result@Full(_)) => result
                                            case None => Failure("invokeMethod " + meth, Empty, Nil)
                                   }
  }

  /**
   * Create a new instance of a class
   *
   * @return a Full can with the instance or a Failure if the instance can't be created
   */
  def instantiate[C](clz: Class[C]): Can[C] = tryo { clz.newInstance }

  /**
   * Create a function (the 'invoker') which will trigger any public, parameterless method
   * That function will throw the cause exception if the method can't be invoked
   *
   * @param clz class whose method should be invoked
   * @param on instance whose method must be invoked
   *
   * @return Empty if instance is null or Full(invoker)
   */
  def createInvoker[C <: AnyRef](name: String, on: C): Can[() => Can[Any]] = {
    def controllerMethods(instance: C) = instance.getClass.getDeclaredMethods.filter { m =>
      m.getName == name && isPublic(m.getModifiers) && m.getParameterTypes.isEmpty
    }
    on match {
      case null => Empty
      case instance => {
        controllerMethods(instance).toList match {
            case Nil => Empty
            case x :: xs => Full(() => {
              try {
                Full(x.invoke(instance))
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
