package net.liftweb.http

/*                                                *\
 (c) 2006 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0

\*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import scala.collection.mutable.{Map, HashMap}
import scala.xml.Elem
import scala.collection.immutable.{ListMap}
import net.liftweb.util.{Helpers, ThreadGlobal}
import net.liftweb.mapper.Safe
import Helpers._
/**
 * An object representing the current state of the HTTP request and response
 * It uses the DynamicVariable construct such that each thread has its own
 * local session info without passing a huge state construct around
 */
object S {
  /**
   * The current session
   */
  private val _request = new ThreadGlobal[RequestState];
  
  /**
   * Get the current HttpServletSession
   *
   * @return the current session
   */
  def request = {_request.value}
  
  /**
   * Test the current request to see if it's a POST
   */
  def post_? = request.post_?
  
  private val executionInfo = new ThreadGlobal[HashMap[String, Function[Array[String], Any]]]
  
  private val currCnt = new ThreadGlobal[int]
  
  def nc : String = {
    val n = currCnt.value
    currCnt := n + 1
    String.format("%06d", Array(new Integer(n)))
  }
  
  /**
  * Initialize the current "State" session
  */
  def init[B](request : RequestState)(f : => B) : B = {
      this._request.doWith(request) {
            this.currCnt.doWith(0)(f)
	}
      }

  /*
  /**
  * Get the value of a variable with:
  * val myVariable = S("foo").asInstanceOf(List[int])
  */
  def apply[T](n : String) : Option[T] = {
    variables.value match {
      case map : Map[_,_] => {
        map.get(n.toLowerCase) match {
          case v @ Some(_) if (v.get.isInstanceOf[T]) => {
            val tv = v.get
            Some(tv.asInstanceOf[T])
          }
          case _ => {None}
        }
      }
      case _ => {None}
    }
  }
  
  /**
  * Set the value of a variable with:
  * S("foo") = 1 :: 2 :: 3 :: Nil
  */
  def update(n : String, v : Any) : Any = {
    if (variables.value != null) {
	v match {
	  case ev @Some(_) => {variables.value += n.toLowerCase -> ev.get}
	  case _ => {variables.value += n.toLowerCase -> v}
	}
    }
    v
  }

  /**
  * "Push" the current variable state.  The current variables are snapshotted and preserved.  When
  * the closure is done executing, the "snapshotted" variables are restored.
  */
  def push[B](f : => B) : B = {
    variables.doWith(variables.value.clone)(f)
  }

  /**
   * Returns a string representation of the variable value or a zero length
   * string if the variable is not found
   */
  def nice(n : String) : String = {
    val v = variables.value.get(n)

    if (v == None || v == null) "" else v.asInstanceOf[Some[Any]].get.toString
  }
  */
    
    def params(n: String) = request.params(n)
    def param(n: String): Option[String] = request.param(n)
}
