package net.liftweb.paypal

import net.liftweb.util._
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.methods._


sealed abstract class PaypalMode
case object PaypalSandbox extends PaypalMode {
  override def toString = "www.sandbox.paypal.com"
}
case object PaypalLive extends PaypalMode {
  override def toString = "www.paypal.com"
}

/* common functionlaity over both IPN and PDT */
trait Paypal {
  
}



/********************
* PAYPAL PDT

var p = PayPalDataTransfer("sandbox").useSSL(false).transactionToken(S.param("tx")).authenticationToken("YOUR_TOKEN")
p.execute
println(p.isSuccessful.toString) // returns true or false depending on whats returned from paypal
********************/
class PayPalDataTransfer extends Paypal {
  /* the target mode, can only be sandbox or live */
  var mode: PaypalMode = PaypalSandbox
  /* if your using SSL, then this will change to https, default is http */
  private var method: String = "http"
  /* the transaction token passed back from paypal */
  var transactionToken: String = ""
  /* your authentication token: avalible from the paypal merchent pannel */
  var authenticationToken: String = ""
  /* port number to make the call on - internal method */
  private var port: Int = 80
  /* HTTP response from paypal */
  private var response: Array[String] = Array()
  /* HTTP client object */
  private val client: HttpClient = new HttpClient()
  
  /* get the party started - do that check */
  def execute: PayPalDataTransfer = { sendAndRecive(pdtPost); this } 
  
  /* convenience method to determine if the transaction was a sucsess */
  def isSuccessful: Boolean = response(0) match {
    case "SUCCESS" => true
    case _ => false
  }
  
  /* make the request over SSL? */
  def useSSL(s: Boolean): PayPalDataTransfer = { 
    s match { 
      case true => { method = "https"; port = 443 }
      case false => { method = "http"; port = 80 }
    }
    this
  }
  /* set the authentication token */
  def authenticationToken(t: String): PayPalDataTransfer = { authenticationToken = t; this }
  
  /* set weather were using live or the sandbox */
  def mode(m: PaypalMode): PayPalDataTransfer = { mode = m; configureClient; this }
  
  def transactionToken(t: Can[String]): PayPalDataTransfer = {
    transactionToken = t.openOr(""); this
  }
  /* the resut string will be mappy back, so this is just a longer way of access
     the various items in that array. */
  private def getResponseItem(idx: Int): String = {
    return response.apply(idx)
  }
  /* update the client object with live/sandbox setting options */
  private def configureClient = client.getHostConfiguration().setHost(mode.toString, port, method)

  /* configure the post object */
  private def pdtPost: PostMethod = {
    val pdtpost: PostMethod = new PostMethod("/cgi-bin/webscr")
    val cmd: NameValuePair = new NameValuePair("cmd", "_notify-synch")
    val tx: NameValuePair = new NameValuePair("tx", transactionToken)
    val auth_token: NameValuePair = new NameValuePair("at", authenticationToken)
    val paramater_array: Array[NameValuePair] = Array(cmd,tx,auth_token)
    pdtpost.setRequestBody(paramater_array)
    return pdtpost
  }
  /* actually instansiate the post! */
  private def sendAndRecive(aPost: PostMethod): Array[String] = {
    client.executeMethod(aPost)
    //println("##### POST: " + aPost.getResponseBodyAsString())
    response = aPost.getResponseBodyAsString().split("\n")
    aPost.releaseConnection()
    return response
  }
}

object PayPalDataTransfer {
  def apply(aMode: PaypalMode): PayPalDataTransfer = {
    var p: PayPalDataTransfer = new PayPalDataTransfer()
    p.mode(aMode)
    p
  }
}

/********************
* PAYPAL IPN
*********************/

class PayPalInstantPaymentNotification {
  
}

object PayPalInstantPaymentNotification {
  
}