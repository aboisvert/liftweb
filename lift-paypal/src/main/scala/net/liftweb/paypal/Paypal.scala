package net.liftweb.paypal

import net.liftweb.util.Helpers
import Helpers._
import _root_.net.liftweb.util._
import _root_.org.apache.commons.httpclient._
import _root_.org.apache.commons.httpclient.methods._
import _root_.java.io._

/**
 * sealed abstract type PaypalMode so we can cast to the super
 * class in our method declerations. Cannot be subclasses outside
 * of this source file.
 */
sealed abstract class PaypalMode

case object PaypalSandbox extends PaypalMode {
  override def toString = "www.sandbox.paypal.com"
}

case object PaypalLive extends PaypalMode {
  override def toString = "www.paypal.com"
}

/**
 * Represents the type of connection that can be made
 * to paypal, irrespecitve of the mode of connection
 */
sealed abstract class PaypalConnection {
  def port = 80
}

case object PaypalHTTP extends PaypalConnection {
  override def toString = "http"
  override def port: Int = 80
}

case object PaypalSSL extends PaypalConnection {
  override def toString = "https"
  override def port: Int = 443
}

/**
 * As the HTTP Commons HttpClient class is by definition very mutable, we
 * provide this factory method to produce an instance we can assign to a val
 *
 * @param url The domain url your sending to
 * @param port The TCP port the message will be sent over
 * @param connection The protocal to use: http, or https
 */
object HttpClientFactory {
  def apply(url: String, port: Int, connection: String): HttpClient = {
    var c: HttpClient = new HttpClient()
    c.getHostConfiguration().setHost(url, port, connection)
    return c
  }
}

object PostMethodFactory {
  def apply(url: String, paramaters: Array[NameValuePair]): PostMethod = {
    var p: PostMethod = new PostMethod(url)
    p.setRequestBody(paramaters)
    return p
  }
}

/**
* A simple abstraction for all HTTP operations. By definition they will return a HTTP error
* code. We are invaribly only concerned with if it was a good one or not.
*/
sealed abstract class PaypalHttpOperation {
  def wasSuccessful(code: Int): Boolean = code match {
    case 200 => true
    case _ => false
  }
}

case class PaypalRequest(client: HttpClient, post: PostMethod) extends PaypalHttpOperation {
  def withClient(in: HttpClient) = PaypalRequest(in, post)
  def withPost(in: PostMethod) = PaypalRequest(client, in)
  def invoke: PaypalResponse = {
    wasSuccessful(tryo(client.executeMethod(post)).openOr(500)) match {
      case true => { 
        RawPostInputStreamSingleton(post)
        PaypalResponse()
      }
      case _ => PaypalFailure("Recived HTTP code other than 200 OK")
    }
  }
}

/**
 * As InputStream is a mutable I/O, we need to use a singleton to access
 * it / process it and return us a immutable result we can work with. If
 * we didnt do this then we get into a whole world of hurt and null pointers.
 */
object RawPostInputStreamSingleton {
  
  private var _outputList: List[String] = List()
  
  def apply(p: PostMethod): List[String] = {
    var stream: InputStream = p.getResponseBodyAsStream()
    val reader: BufferedReader = new BufferedReader(new InputStreamReader(stream))
    if(_outputList.length == 0){
      var line: String = null
      try {
        while ({line = reader.readLine(); line != null}){
          //println("########## " + line)
          _outputList = _outputList ::: List(line)
        }
      } finally {
        if(reader != null){
          reader.close
        }
      }
    }
    _outputList
  }
  
  def apply(): List[String] = _outputList
}

case class PaypalResponse extends PaypalHttpOperation {

  def getValueForKey(k: String): String = splitParamaterToKeyValuePair(
        RawPostInputStreamSingleton().filter(splitParamaterToKeyValuePair(_)(0) == k).toString)(1)
  
  def getRawValueAtIndex(idx: Int): String = RawPostInputStreamSingleton()(idx)
  
  protected def splitParamaterToKeyValuePair(in: String) = in.split("=").toList
  
}

/**
 * Wrapper for a failure being returned from paypal. 
 *
 * @param post The instance of PostMethod from HTTP Commons lib
 * @param message Specifiy if you want to detail the error that actually occoured
 */
case class PaypalFailure(message: String) extends PaypalResponse
object PaypalFailure {
  def apply(): PaypalFailure = PaypalFailure("Generic Failure")
}

/**
 * Common functionality for paypal PDT and IPN
 *
 * @param mode The PaypalMode type that your targeting. Options are PaypalLive or PaypalSandbox
 * @param connection The protocol the invocation is made over. Options are PaypalHTTP or PaypalSSL
 */
abstract class Paypal(val mode: PaypalMode, val connection: PaypalConnection){
  val client: HttpClient = HttpClientFactory(mode.toString, connection.port, connection.toString)
}

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
case class PaypalDataTransfer(authToken: String, transactionToken: String, override val mode: PaypalMode, override val connection: PaypalConnection) extends Paypal(mode: PaypalMode, connection: PaypalConnection) {
  /**
   * payloadArray is the array of the post body we'll be sending.
   * As the payload body is different in PDT vs IPN
   */
  val payloadArray: Array[NameValuePair] = Array(
    new NameValuePair("cmd", "_notify-synch"),
    new NameValuePair("tx", transactionToken),
    new NameValuePair("at", authToken)
  )
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
    PaypalRequest(client,PostMethodFactory("/cgi-bin/webscr",payloadArray)).invoke
  )
}

object PaypalDataTransfer {
  /**
  * Builder method to create a new PDT instance
  * 
  * @param authToken The token you obtain from the paypal merchant console
  * @param transactionToken The token that is passed back to your application as the "tx" part of the query string
  */
  def apply(authToken: String, transactionToken: String): PaypalDataTransfer = PaypalDataTransfer(authToken, transactionToken, PaypalSandbox, PaypalHTTP)
}

/**
 * Wrapper instance for handling the response from a paypal data transfer.
 * 
 * @param post The actually PostMethod were concerned with here
 */
case class PaypalDataTransferReponse(response: PaypalResponse) extends PaypalResponse {
  /**
  * Quick utility method for letting you know if the payment data is returning a sucsessfull message
  *
  * @return Boolean
  */
  def paymentSuccessful: Boolean = getRawValueAtIndex(0) match {
   case "SUCCESS" => true
   case _ => false
  }
}

/**
 * In response to the IPN postback from paypal, its nessicary to then call paypal and pass back
 * the exact set of paramaters that you were given by paypal - this stops spoofing. Use the 
 * PaypalInstantPaymentTransferPostback exactly as you would PaypalDataTransferReponse.
 */
case class PaypalInstantPaymentTransferPostback(override val mode: PaypalMode, override val connection: PaypalConnection) extends Paypal(mode: PaypalMode, connection: PaypalConnection) {
  val payloadArray: Array[NameValuePair] = Array(
    new NameValuePair("cmd", "_notify-validate")
  )
}

