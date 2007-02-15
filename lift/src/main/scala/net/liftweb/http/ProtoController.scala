package net.liftweb.http

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */
  
import scala.collection.immutable.TreeMap
  
/**
  * The base trait of Controllers that handle pre-view requests
  */
trait ProtoController {
  var request: RequestState = _
  var session = TreeMap.empty[String, Any]
                          
  def params(name: String): Option[String] = {
    request.params.get(name) match {
      case None => None
      case Some(nl) => nl.take(1) match {
        case Nil => None
        case l => Some(l.head)
      }
    }
  }
  
  def post_? : boolean = request.post_?
  
  /*
  def apply(name: String): Option[Any] = {
    session.get(name)
  }*/
  
  def apply[T](name: String): Option[T] = {
    session.get(name) match {
      case None => None
      case Some(n) => {
        if (n.isInstanceOf[T]) Some(n.asInstanceOf[T])
        else None
      }
    }
  }
  
  def update(name: String, value: Any) {
    session = (session(name) = value)
    // FIXME send the change to the actual session
  }
}
