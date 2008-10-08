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
/**
 * type object for the paypal sandbox endpoint 
 */
case object PaypalSandbox extends PaypalMode {
  override def toString = "www.sandbox.paypal.com"
}
/**
 * type object for the paypal live endpoint 
 */
case object PaypalLive extends PaypalMode {
  override def toString = "www.paypal.com"
}

/**
 *
 */
sealed abstract class PaypalConnection
case object PaypalHTTP extends PaypalConnection {
  override def toString = "http"
  def port: Int = 80
}
case object PaypalSSL extends PaypalConnection {
  override def toString = "https"
  def port: Int = 443
}


/**
 * Common functionality for paypal PDT and IPN
 */
abstract class Paypal(val mode: PaypalMode, val connection: PaypalConnection) {
  protected val client: HttpClient = new HttpClient()
  protected val payload: PostMethod = new PostMethod("/cgi-bin/webscr")
  
  def wasSuccessful(code: Int): Boolean = code match {
    case 200 => true
    case _ => false
  }
  
/*  protected def response: String = {
    val buffer: Can[BufferedReader] = tryo(new InputStreamReader(
       payload.getResponseBodyAsStream()
    )).openOr("ERROR")
  }*/
  
  private def execute: Can[Int] = tryo(
    client.executeMethod(payload)
  )
  
}
/**
 * If you need to get data from paypal PDT, simply instansiate this class 
 * (through one of the factory objects)
 */
case class PaypalDataTransfer(authToken: String, transactionToken: String, 
                              override val mode: PaypalMode, override val connection: PaypalConnection) 
                              extends Paypal(mode: PaypalMode, connection: PaypalConnection) {
  
  /**
  * @param in set the endpoint for the connection. Must be a subclass of PaypalMode.
  */
  def withMode(in: PaypalMode): PaypalDataTransfer = PaypalDataTransfer(authToken, transactionToken, 
                                                                        in, connection)
  /**
   * @param in set weather or not the connection should be made over SSL/443/https or not.
   */
  def withConnection(in: PaypalConnection): PaypalDataTransfer = PaypalDataTransfer(authToken, transactionToken, mode, in)
  
}
object PaypalDataTransfer {
  def apply(authToken: String, transactionToken: String): PaypalDataTransfer =
    PaypalDataTransfer(authToken, transactionToken, PaypalSandbox, PaypalHTTP)
}




