package net.liftweb.http

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0

\*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import scala.collection.mutable.{Map, HashMap, ArrayBuffer}
import scala.xml.{NodeSeq, Elem, Text}
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
  private val _servletRequest = new ThreadGlobal[HttpServletRequest];
  private val functionMap = new ThreadGlobal[HashMap[String, (List[String]) => boolean]];
  private val _notice = new ThreadGlobal[ArrayBuffer[(NoticeType.Value, NodeSeq)]];
  private val _oldNotice = new ThreadGlobal[Seq[(NoticeType.Value, NodeSeq)]];
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
  
  private def initNotice[B](f: => B): B = {
    _notice.doWith(new ArrayBuffer[(NoticeType.Value, NodeSeq)])(f)
  }
  
  /**
   * Initialize the current "State" session
  */
  def init[B](request : RequestState)(f : => B) : B = {
    _oldNotice.doWith(Nil) {
    initNotice {
    functionMap.doWith(new HashMap[String, (List[String]) => boolean]) {
      this._request.doWith(request) {
        this.currCnt.doWith(0)(f)
      }
    }
    }
    }
  }
  
  def init[B](request: RequestState, servletRequest: HttpServletRequest, oldNotices: Seq[(NoticeType.Value, NodeSeq)])(f : => B) : B = {
    _oldNotice.doWith(oldNotices) {
    initNotice {
    functionMap.doWith(new HashMap[String, (List[String]) => boolean]) {
      this._servletRequest.doWith(servletRequest) {
	this._request.doWith(request) {
          this.currCnt.doWith(0)(f)
	}
      }
    }
    }
    }
  }
  
  def get(what: String): Option[String] = {
    _servletRequest.value match {
      case null => None
      case s => s.getSession.getAttribute(what) match {
	case null => None
	case n => {
	  if (n.isInstanceOf[String]) Some(n.asInstanceOf[String])
	  else None
	}
      }
    }
  }
  
  def set(name: String, value: Option[String]) {
    if (_servletRequest.value ne null) {
      value match {
	case None => _servletRequest.value.getSession.removeAttribute(name)
	case Some(vlue) => {_servletRequest.value.getSession.setAttribute(name, vlue)}
      }
    }
  }
  
  def getFunctionMap: Map[String, (List[String]) => boolean] = functionMap.value

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
    
  def f(in: List[String] => boolean): String = {
    val name = "F"+System.nanoTime+"_"+randomString(3)
    addFunctionMap(name, in)
    name
  }
  
  def f(name: String, inf: List[String] => boolean): String = {
    addFunctionMap(name, inf)
    name
  }
  
  def addFunctionMap(name: String, value: (List[String]) => boolean) {
    functionMap.value += (name -> value)
  }
  def mapFunction(name: String, f: (List[String]) => boolean): String = {
    val ret = ""+nc+"_"+name+"_"+randomString(5)
    functionMap.value += (ret -> f)
    ret
  }
  
  def params(n: String) = request.params(n)
  def param(n: String): Option[String] = request.param(n)
  
  def error(n: String) {error(Text(n))}
  def error(n: NodeSeq) {_notice.value += (NoticeType.Error, n)}
  def notice(n: String) {notice(Text(n))}
  def notice(n: NodeSeq) {_notice.value += (NoticeType.Notice, n)}
  def warning(n: String) {warning(Text(n))}
  def warning(n: NodeSeq) {_notice.value += (NoticeType.Warning, n)}
  
  def getNotices = _notice.value.toList
  
  def errors: Seq[NodeSeq] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Error).map(_._2))
  def notices: Seq[NodeSeq] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Notice).map(_._2))
  def warnings: Seq[NodeSeq] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Warning).map(_._2))
  def clearCurrentNotices {_notice.value.clear}
  
}

object NoticeType extends Enumeration {
  val Notice, Warning, Error = Value
}
