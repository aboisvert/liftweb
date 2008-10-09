package net.liftweb.paypal

import net.liftweb.util.Helpers
import Helpers._
import net.liftweb.util._
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.methods._
import java.io.{InputStream,BufferedReader,InputStreamReader}

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
      case true => PaypalResponse(post)
      case _ => PaypalFailure(post).withMessage("Recived HTTP code other than 200 OK")
    }
  }
}
case class PaypalResponse(post: PostMethod) extends PaypalHttpOperation {
  val responseStream: InputStream = post.getResponseBodyAsStream()
  def getProcessedResponse: Can[String] = {
    var _readStream: BufferedReader = new BufferedReader(
      new InputStreamReader(responseStream)
    )
    val _result: Can[String] = tryo(_readStream.readLine())
    _readStream.close()
    println("######### L81:" + _result.openOr(""))
    return _result
  }
}

/**
 * Wrapper for a failure being returned from paypal. 
 *
 * @param post The instance of PostMethod from HTTP Commons lib
 * @param message Specifiy if you want to detail the error that actually occoured
 */
case class PaypalFailure(override val post: PostMethod, message: String) extends PaypalResponse(post: PostMethod) {
  def withMessage(in: String) = PaypalFailure(post, in)
}
object PaypalFailure {
  def apply(p: PostMethod): PaypalFailure = PaypalFailure(p, "Generic Failure")
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
    PaypalRequest(client,PostMethodFactory("/cgi-bin/webscr",payloadArray)).invoke.post
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

case class PaypalDataTransferReponse(override val post: PostMethod) extends PaypalResponse(post: PostMethod) {
  /**
  * Quick utility method for letting you know if the payment data is returning a sucsessfull message
  *
  * @return Boolean
  */
  def paymentSuccessful: Boolean = getProcessedResponse.openOr("") match {
    case "SUCSESS" => true
    case _ => false
  }
}

