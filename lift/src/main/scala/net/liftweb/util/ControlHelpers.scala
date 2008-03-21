package net.liftweb.util

/**
 * Control helpers are providing alternate ways to catch exceptions and ignore them as necessary
 */
trait ControlHelpers extends ClassHelpers {
 
  /**
   * Wraps a "try" block around the function f. If f throws
   * an exception with it's class in 'ignore' or of 'ignore' is
   * null or an empty list, ignore the exception and return None.
   * 
   * @param ignore list of exception classes to ignore. A thrown exception will be ignored if it is assignable from one of
   * the exception class in the list
   * @param onError optional callback function that will use the thrown exception as a parameter
   * @param f block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception 
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: List[Class[_]], onError: Can[Throwable => Unit])(f: => T): Can[T] = {
    try {
      Full(f)
    } catch {
      case c if (containsClass(c.getClass, ignore)) => onError.foreach(_(c)); Empty
      case c if (ignore == null || ignore.isEmpty) => onError.foreach(_(c)); Failure("tryo", Full(c), Nil)
    }
  }
  
  /**
   * Wraps a "try" block around the function f
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception 
   *   <li>a Failure if f throws an exception
   *   </ul>
   */
  def tryo[T](f: => T): Can[T] = tryo(Nil, Empty)(f)
  
  
  /**
   * Wraps a "try" block around the function f and trigger a callback function if an exception is thrown
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception 
   *   <li>a Failure if f throws an exception
   *   </ul>
   */
  def tryo[T](onError: Throwable => Unit)(f: => T): Can[T] = tryo(Nil, Full(onError))(f)  

  /**
   * Wraps a "try" block around the function f
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception 
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: List[Class[_]])(f: => T): Can[T] = tryo(ignore, Empty)(f)

  /**
   * Wraps a "try" block around the function f. Takes only one Class of exception to ignore
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception 
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: Class[_])(f: => T): Can[T] = tryo(List(ignore), Empty)(f)
}

    
