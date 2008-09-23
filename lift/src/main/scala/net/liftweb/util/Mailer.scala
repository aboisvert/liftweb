package net.liftweb.util

/*                                                *\
(c) 2007 WorldWide Conferencing, LLC
Distributed under an Apache License
http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import _root_.scala.xml.{NodeSeq}
import _root_.scala.actors._
import Actor._
import _root_.javax.mail._
import _root_.javax.mail.internet._

/**
* Send email
*/
object Mailer {
  sealed abstract class MailTypes
  sealed abstract class MailBodyType extends MailTypes
  case class PlusImageHolder(name: String, mimeType: String, bytes: Array[Byte])

  case class PlainMailBodyType(text: String) extends MailBodyType
  case class XHTMLMailBodyType(text: NodeSeq) extends MailBodyType
  case class XHTMLPlusImages(text: NodeSeq, items: PlusImageHolder*) extends MailBodyType

  sealed abstract class RoutingType extends MailTypes
  sealed abstract class AddressType(val adr: String) extends RoutingType
  case class From(address: String) extends AddressType(address)
  case class To(address: String) extends AddressType(address)
  case class CC(address: String) extends AddressType(address)
  case class Subject(subject: String) extends RoutingType
  case class BCC(address: String) extends AddressType(address)
  case class ReplyTo(address: String) extends AddressType(address)

  implicit def stringToMailBodyType(text: String): MailBodyType = PlainMailBodyType(text)
  implicit def xmlToMailBodyType(html: NodeSeq): MailBodyType = XHTMLMailBodyType(html)

  case class MessageInfo(from: From, subject: Subject, info: List[MailTypes])

  implicit def addressToAddress(in: AddressType): Address = new InternetAddress(in.adr)
  implicit def adListToAdArray(in: List[AddressType]): Array[Address] = in.map(a => new InternetAddress(a.adr)).toArray


  /**
   * Passwords cannot be accessed via System.getProperty.  Instead, we
   provide a means of explicitly
   * setting the authenticator.
   */
  //def authenticator = authenticatorFunc
  var authenticator: Can[Authenticator] = Empty


  /**
  * What host should be used to send mail
  */
  def host = hostFunc()

  /**
  * To change the way the host is calculated, set this to the function that calcualtes the host name.
  * By default: System.getProperty("mail.smtp.host")
  */
  var hostFunc: () => String = _host _

  private def _host = System.getProperty("mail.smtp.host") match {
    case null => "localhost"
    case s => s
  }
  // def host_=(hostname: String) = System.setProperty("mail.smtp.host", hostname)

  private class MsgSender extends Actor {
    def act = {
      loop {
        react {
          case MessageInfo(from, subject, info) =>
          try {
	    val session =
	      authenticator match {
		case Full(a) => Session.getInstance(System.getProperties, a)

		case _ => Session.getInstance(System.getProperties)
	      }
            val message = new MimeMessage(session)
            message.setFrom(from)
            message.setRecipients(Message.RecipientType.TO, info.flatMap{case x: To => Some[To](x) case _ => None})
            message.setRecipients(Message.RecipientType.CC, info.flatMap{case x: CC => Some[CC](x) case _ => None})
            message.setRecipients(Message.RecipientType.BCC, info.flatMap{case x: BCC => Some[BCC](x) case _ => None})
            // message.setReplyTo(filter[MailTypes, ReplyTo](info, {case x @ ReplyTo(_) => Some(x); case _ => None}))
            message.setReplyTo(info.flatMap{case x: ReplyTo => Some[ReplyTo](x) case _ => None})
            message.setSubject(subject.subject)
            val multiPart = new MimeMultipart("alternative")
            info.flatMap{case x: MailBodyType => Some[MailBodyType](x); case _ => None}.foreach {
              tab =>
              val bp = new MimeBodyPart
              tab match {
                case PlainMailBodyType(txt) => bp.setContent(txt, "text/plain")
                case XHTMLMailBodyType(html) => bp.setContent(html.toString, "text/html")
                case XHTMLPlusImages(html, img @ _*) =>
                val html_mp = new MimeMultipart("related")
                val bp2 = new MimeBodyPart
                bp2.setContent(html.toString, "text/html")
                html_mp.addBodyPart(bp2)
                img.foreach {
                  i =>
                  val rel_bpi = new MimeBodyPart
                  rel_bpi.setFileName(i.name)
                  rel_bpi.setContentID(i.name)
                  rel_bpi.setDisposition("inline")
                  rel_bpi.setDataHandler(new _root_.javax.activation.DataHandler(new _root_.javax.activation.DataSource{
                    def getContentType = i.mimeType
                    def getInputStream = new _root_.java.io.ByteArrayInputStream(i.bytes)
                    def getName = i.name
                    def getOutputStream = throw new _root_.java.io.IOException("Unable to write to item")
                  }))
                  html_mp.addBodyPart(rel_bpi)
                }
                bp.setContent(html_mp)
              }
              multiPart.addBodyPart(bp)
            }
            message.setContent(multiPart);

            Transport.send(message);
          } catch {
            case e: Exception => Log.error("Couldn't send mail", e)
          }

          case _ => Log.warn("Email Send: Here... sorry")
        }
      }
    }
  }


  private val msgSender = {
    val ret = new MsgSender
    ret.start
    ret
  }

  def sendMail(from: From, subject: Subject, rest: MailTypes*) {
    // forward it to an actor so there's no time on this thread spent sending the message
    msgSender ! MessageInfo(from, subject, rest.toList)
  }

}
