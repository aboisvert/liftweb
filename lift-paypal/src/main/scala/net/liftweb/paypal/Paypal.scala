package net.liftweb.paypal

import _root_.net.liftweb.util.Helpers
import Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.org.apache.commons.httpclient._
import _root_.org.apache.commons.httpclient.methods._
import _root_.java.io._

import _root_.scala.collection.mutable.ListBuffer

import _root_.scala.actors.Actor
import Actor._

/**
 * sealed abstract type PaypalMode so we can cast to the super
 * class in our method declerations. Cannot be subclasses outside
 * of this source file.
 */
sealed trait PaypalMode {
  def domain: String
  override def toString = "PaypalMode: "+domain
}

object PaypalSandbox extends PaypalMode {
  def domain = "www.sandbox.paypal.com"
}

object PaypalLive extends PaypalMode {
  def domain = "www.paypal.com"
}

/**
 * Represents the type of connection that can be made
 * to paypal, irrespecitve of the mode of connection
 */
sealed trait PaypalConnection {
  def protocol: String
  def port: Int = 80
  
  override def toString = "PaypalConnection: "+protocol+":"+port
}

object PaypalHTTP extends PaypalConnection {
  def protocol = "http"
}

object PaypalSSL extends PaypalConnection {
  def protocol = "https"
  override def port: Int = 443
}

object PaypalTransactionStatus extends Enumeration {
  val CanceledPayment = Value(1, "Canceled")
  val ClearedPayment = Value(2, "Cleared")
  val CompletedPayment = Value(3, "Completed")
  val DeniedPayment = Value(4, "Denied")
  val ExpiredPayment = Value(5, "Expired")
  val FailedPayment = Value(6, "Failed")
  val PendingPayment = Value(7, "Pending")
  val RefundedPayment = Value(8, "Refunded")
  val ReturnedPayment = Value(9, "Returned")
  val ReversedPayment = Value(10, "Reversed")
  val UnclaimedPayment = Value(11, "Unclaimed")
  val UnclearedPayment = Value(12, "Uncleared")
}


/**
 * As the HTTP Commons HttpClient class is by definition very mutable, we
 * provide this factory method to produce an instance we can assign to a val
 *
 * @param url The domain url your sending to
 * @param port The TCP port the message will be sent over
 * @param connection The protocal to use: http, or https
 */
private object HttpClientFactory {
  def apply(url: String, port: Int, connection: String): HttpClient = {
    val c: HttpClient = new HttpClient()
    c.getHostConfiguration().setHost(url, port, connection)
    c
  }
}

private object PostMethodFactory {
  def apply(url: String, paramaters: Seq[(String, String)]): PostMethod = {
    val p: PostMethod = new PostMethod(url)
    p.setRequestBody(paramaters)
    p
  }
  
  implicit def tonvp(in: Seq[(String, String)]): Array[NameValuePair] = 
  in.map(p => new NameValuePair(p._1, p._2)).toArray
}

/**
 * Common functionality for paypal PDT and IPN
 *
 * @param mode The PaypalMode type that your targeting. Options are PaypalLive or PaypalSandbox
 * @param connection The protocol the invocation is made over. Options are PaypalHTTP or PaypalSSL
 */

trait PaypalBase {
  protected def client(mode: PaypalMode, connection: PaypalConnection): HttpClient = HttpClientFactory(mode.domain, connection.port, connection.protocol)
  // def mode: PaypalMode
  //def connection: PaypalConnection
  // def payloadArray: Seq[(String, String)]
}


/**
* A simple abstraction for all HTTP operations. By definition they will return a HTTP error
* code. We are invaribly only concerned with if it was a good one or not.
*/
trait PaypalUtilities {
  def wasSuccessful(code: Int): Boolean = code match {
    case 200 => true
    case _ => false
  }
}

/**
 * All HTTP requests to the paypal servers must subclass PaypalRequest.
 * 
 * @param client Must be a HTTP client; the simplest way to create this is by using HttpClientFactory
 * @param post Specify the payload of the HTTP request. Must be an instance of PostMethod from HTTP commons
 */
object PaypalRequest extends PaypalUtilities {
  //def withClient(in: HttpClient) = PaypalRequest(in, post)
  //def withPost(in: PostMethod) = PaypalRequest(client, in)
  def apply(client: HttpClient, post: PostMethod): List[String] = wasSuccessful(tryo(client.executeMethod(post)).openOr(500)) match {
    case true => StreamResponseProcessor(post)
    case _ => List("Failure")
  }
}

/**
 * As InputStream is a mutable I/O, we need to use a singleton to access
 * it / process it and return us a immutable result we can work with. If
 * we didnt do this then we get into a whole world of hurt and null pointers.
 */
private object StreamResponseProcessor {
  def apply(p: PostMethod): List[String] = {
    val stream: InputStream = p.getResponseBodyAsStream()
    val reader: BufferedReader = new BufferedReader(new InputStreamReader(stream))
    val ret: ListBuffer[String] = new ListBuffer
    
    try {
      def doRead {
         reader.readLine() match {
           case null => ()
           case line => ret += line
         }
      }
      
      doRead
      ret.toList
    } catch {
      case _ => Nil
    }    
  }
}


/**
 * All paypal service classes need to subclass PaypalResponse explicitally. 
 */
trait PaypalResponse extends PaypalUtilities {
  def response: List[String]
  
  def getValueForKey(k: String): String = splitParamaterToKeyValuePair(
        response.filter(
          splitParamaterToKeyValuePair(_)(0) == k).toString)(1)
  
  def rawHead: Can[String] = Can(response.firstOption)
  
  protected def splitParamaterToKeyValuePair(in: String) = in.split("=").toList
  
}

/**
 * Wrapper for a failure being returned from paypal. 
 *
 * @param post The instance of PostMethod from HTTP Commons lib
 * @param message Specifiy if you want to detail the error that actually occoured
 */
// case class PaypalFailure(message: String) extends PaypalResponse
// object PaypalFailure {
//   def apply(): PaypalFailure = PaypalFailure("Generic Failure")
// }

//PAYPAL PDT

/**
 * If you need to get data from paypal PDT, simply instansiate this class 
 * (through one of the factory objects)
 *
 * @param authToken The token you obtain from the paypal merchant console
 * @param transactionToken The token that is passed back to your application as the "tx" part of the query string
 * @param mode The PaypalMode type that your targeting. Options are PaypalLive or PaypalSandbox
 * @param connection The protocol the invocation is made over. Options are PaypalHTTP or PaypalSSL
 * @return PaypalDataTransfer
 */
case class PaypalDataTransfer(authToken: String, transactionToken: String, mode: PaypalMode, connection: PaypalConnection) extends PaypalBase {
  /**
   * payloadArray is the array of the post body we'll be sending.
   * As the payload body is different in PDT vs IPN
   */
  def payloadArray = List("cmd" -> "_notify-synch",
    "tx" -> transactionToken,
    "at" -> authToken)
  
  /**
  * @param in set the endpoint for the connection. Must be a subclass of PaypalMode.
  */
  def withMode(in: PaypalMode): PaypalDataTransfer = PaypalDataTransfer(authToken, transactionToken, in, connection)
  /**
   * @param in set weather or not the connection should be made over SSL/443/https or not.
   */
  def withConnection(in: PaypalConnection): PaypalDataTransfer = PaypalDataTransfer(authToken, transactionToken, mode, in)
  /**
  * @return PaypalDataTransferReponse
  */
  def execute: PaypalDataTransferReponse = PaypalDataTransferReponse(
    PaypalRequest(client(mode, connection),PostMethodFactory("/cgi-bin/webscr",payloadArray))
  )
}

/*
object PaypalDataTransfer {
  /**
  * Builder method to create a new PDT instance
  * 
  * @param authToken The token you obtain from the paypal merchant console
  * @param transactionToken The token that is passed back to your application as the "tx" part of the query string
  */
  def apply(authToken: String, transactionToken: String): PaypalDataTransfer = PaypalDataTransfer(authToken, transactionToken, PaypalSandbox, PaypalHTTP)
}
*/

/**
 * Wrapper instance for handling the response from a paypal data transfer.
 * 
 * @param response The processed response List[String]. The response 
 * input should be created with StreamResponseProcessor
 */
case class PaypalDataTransferReponse(response: List[String]) extends PaypalResponse {
  /**
  * Quick utility method for letting you know if the payment data is returning a sucsessfull message
  *
  * @return Boolean
  */
  def paymentSuccessful: Boolean = rawHead match {
   case Full("SUCCESS") => true
   case _ => false
  }
}

//
// PAYPAL IPN
//

//case class PaypalEventAction(status: PaypalTransactionStatus.Value, functions: List[() => Any])

/**
 * Users would generally invoke this case class in a DispatchPf call in 
 * Boot.scala as it handles the incomming request and dispatches the IPN
 * callback, and handles the subsequent response.
 */
object PaypalIPN extends PaypalUtilities { 
  //actions.map(_.functions.map(_()))
  //println("##############" + request.params)
  // private val ipnStatus: PaypalTransactionStatus
  
  /**
   * @todo Really need to make sure that multiple custom paramaters can be mapped through.
   * The current solution is not good!
   */
  private def paramsAsPayloadList(request: RequestState): Seq[(String, String)] =
    (for(val p <- request.params) yield (p._1, p._2.head)).toList
  
  
  //def onEvent(in: PaypalEventAction) = PaypalIPN(request, actions ++ List(in), mode, connection)
  
  
  /**
  * @param in set the endpoint for the connection. Must be a subclass of PaypalMode.
  */
  //def withMode(in: PaypalMode): PaypalIPN = PaypalIPN(request, actions, in, connection)
  /**
   * @param in set weather or not the connection should be made over SSL/443/https or not.
   */
  //def withConnection(in: PaypalConnection): PaypalIPN = PaypalIPN(request, actions, mode, in)
  
  
  def apply(request: RequestState, mode: PaypalMode, connection: PaypalConnection) = {
    //create request, get response and pass response object to the specified event handlers
    val ipnResponse: PaypalIPNPostbackReponse = PaypalIPNPostback(mode, connection, paramsAsPayloadList(request))
    println(ipnResponse)
    ipnResponse
  }  
}

/*
object PaypalIPN {
  def apply(r: RequestState): PaypalIPN = PaypalIPN(r, List(), PaypalSandbox, PaypalHTTP)
}*/


/**
 * In response to the IPN postback from paypal, its nessicary to then call paypal and pass back
 * the exact set of paramaters that you were given by paypal - this stops spoofing. Use the 
 * PaypalInstantPaymentTransferPostback exactly as you would PaypalDataTransferReponse.
 */
object PaypalIPNPostback extends PaypalBase {
  
  def payloadArray(paramaters: Seq[(String, String)]) = List("cmd" -> "_notify-validate") ++ paramaters
  
  /**
  * @param in Set the payload paramaters to be posted back. This will *always* be the list posted to 
  * your application from paypal IPN service.
  */
  // def withPostbackParamaters(in: Array[NameValuePair]): PaypalIPNPostback = PaypalIPNPostback(mode, connection, in)
  
  /**
  * @return PaypalDataTransferReponse
  */
  def apply(mode: PaypalMode, connection: PaypalConnection, paramaters: Seq[(String, String)]): PaypalIPNPostbackReponse = PaypalIPNPostbackReponse(
    PaypalRequest(client(mode, connection),PostMethodFactory("/cgi-bin/webscr",payloadArray(paramaters)))
  )
}

case class PaypalIPNPostbackReponse(response: List[String]) extends PaypalResponse {
  def isVerified: Boolean = rawHead match {
   case Full("VERIFIED") => true
   case _ => false
  }
}

trait PayPal {
  val RootPath = "paypal"
  val IPNPath = "ipn"
  val PDTPath = "pdt"

  val mode: PaypalMode = PaypalSandbox

  val connection: PaypalConnection = PaypalSSL

  def defaultResponse(): Can[LiftResponse] = Full(PlainTextResponse("ok"))
  
  def dispatch: LiftRules.DispatchPf = {
    case r @ RequestState(RootPath :: IPNPath :: Nil, "", PostRequest) =>
      requestQueue ! IPNRequest(r)
      defaultResponse _
      
      
  }

  private case class IPNRequest(r: RequestState)
  
  private object requestQueue extends Actor {
    def act = {
      loop {
        react {
          case IPNRequest(r) =>
            val resp = PaypalIPN(r, mode, connection)
            if (resp.isVerified) {
              // FIXME process the response
            }
          case _ =>
        }
      }
    }
  }
  requestQueue.start
}