package net.liftweb.util

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

/**
 * A class that does lazy evaluation
 *
 * @param f -- a function that evaluates to the default value of the instance
 */
class Lazy[T](f: => T) {
  private var value: Can[T] = Empty
  
  /**
   * Get the value of the instance.  If it's not yet been set, call f to calculate it
   *
   * @return the value of the instance
   */
  def get: T = {
    value match {
      case Full(v) => v
      case _ => value = Full(f)
      value.open
    }
  }
  
  def defined_? = {
    value != None
  }
  
  /**
   * Set the instance to a new value and return that value
   *
   * @param v - the new value of the instance
   *
   * @return v
   */
  def set(v: T): T = {
    value = Full(v)
    v
  }
  
  /**
   * The Pascal style setter
   */
  def :=(v: T): T = set(v)
  
  /**
   * and the lazy() = foo style of assignment
   */
  def update(v: T): Unit = set(v)
  
  def reset = value = Empty
  
  def calculated_? = value.isDefined
  
  // implicit def fromLazy[T](in: Lazy[T]): T = in.get
}

object Lazy {
  // implicit def fromLazy[T](in: Lazy[T]): T = in.get
  def apply[T](f: => T) = new Lazy(f)
}
