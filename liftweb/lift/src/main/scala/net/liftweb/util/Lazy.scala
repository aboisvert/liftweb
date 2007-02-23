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
  private var value: Option[T] = None
  
  /**
    * Get the value of the instance.  If it's not yet been set, call f to calculate it
    *
    * @return the value of the instance
    */
  def get: T = {
    value match {
      case Some(v) => v
      case None => val v = f
        value = Some(v)
        v
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
    value = Some(v)
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
  
  // implicit def fromLazy[T](in: Lazy[T]): T = in.get
}

object Lazy {
  implicit def fromLazy[T](in: Lazy[T]): T = in.get
  def apply[T](f: => T) = new Lazy(f)
  
}
