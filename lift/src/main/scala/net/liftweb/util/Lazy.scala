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
class FatLazy[T](f: => T) {
  private var value: Can[T] = Empty
  
  /**
   * Get the value of the instance.  If it's not yet been set, call f to calculate it
   *
   * @return the value of the instance
   */
  def get: T = synchronized {
    value match {
      case Full(v) => v
      case _ => value = Full(f)
      value.open_!
    }
  }
  
  def defined_? = synchronized {
    value != None
  }
  
 
  /**
   * Set the instance to a new value and return that value
   *
   * @param v - the new value of the instance
   *
   * @return v
   */
  def set(v: T): T = synchronized {
    value = Full(v)
    v
  }
  
  def setFrom(other: FatLazy[T]): Unit = synchronized {
    value = other.value
  }
  
  /**
   * and the lazy() = foo style of assignment
   */
  def update(v: T): Unit = set(v)
    
  def reset = synchronized {value = Empty}
  
  def calculated_? = synchronized {value.isDefined}
  
  // implicit def fromLazy[T](in: Lazy[T]): T = in.get
}

object FatLazy {
  // implicit def fromLazy[T](in: Lazy[T]): T = in.get
  def apply[T](f: => T) = new FatLazy(f)
}

object ThreadLazy {
  def apply[T](f: => T) = new ThreadLazy(f)
  
  implicit def what[T](in: ThreadLazy[T]): T = in.get
}

class ThreadLazy[TheType](theFunc: => TheType) extends LoanWrapper {
  private val calced = new ThreadGlobal[Boolean]
  private val value = new ThreadGlobal[TheType]
  
  def apply[T](f: => T): T = {
    val old = value.value
    calced.set(false)
    try {
      f
    } finally {
      calced.set(false)
      value.set(old)
    }
  }
  
  def reset(): Unit = calced.set(false)
  
  def get: TheType = {
    if (calced.value) value.value
    else {
      value.set(theFunc)
      calced.set(true)
      value.value
    }
  }
  
}

