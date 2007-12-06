package net.liftweb.amqp

import com.rabbitmq.client._
import scala.actors.Actor
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream

/**
 * @param a The actor to add as a Listener to this Dispatcher.
 */
case class AMQPAddListener(a: Actor)

/**
 * @param message A deserialized value received via AMQP.
 *
 * Messages received from AMQP are wrapped in this case class. When you
 * register a listener, this is the case class that you will be matching on.
 */
case class AMQPMessage[T](message: T)

/**
 * An actor that serves as an endpoint for AMQP messages of serialized type T 
 * coming into a specific queue/exchange. 
 *
 * To listen for messages coming into that queue/exchange, send 
 * this actor an AMQPAddListener message.
 *
 * For each message containing a value of type T, all listeners will be send
 * an AMQPMessage contaning that value.
 *
 * See also Enterprise Integration Patterns pp. 508-514
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
abstract class AMQPDispatcher[T](cf: ConnectionFactory, host: String, port: Int) extends Actor {
  val conn = cf.newConnection(host, port)
  val channel = conn.createChannel()
  configure(channel)

  /**
   * Override this to configure the Channel and Consumer.
   */
  def configure(channel: Channel)

  def act = loop(Nil)

  def loop(as: List[Actor]) {
    react {
      case AMQPAddListener(a) => loop(a :: as)
      case msg@AMQPMessage(t) => as.foreach(_ ! msg); loop(as)
      case _ => loop(as)
    }
  }
}

/**
 *
 */
class SerializedConsumer[T](channel: Channel, a: Actor) extends DefaultConsumer(channel) {
  override def handleDelivery(tag: String, env: Envelope, props: AMQP.BasicProperties, body: Array[byte]) {
    val routingKey = env.getRoutingKey
    val contentType = props.contentType
    val deliveryTag = env.getDeliveryTag
    val in = new ObjectInputStream(new ByteArrayInputStream(body))
    val t = in.readObject.asInstanceOf[T];
    // Send t to all registered listeners.
    a ! AMQPMessage(t)
    channel.basicAck(deliveryTag, false);
  }
}

/**
 * Example Dispatcher that listens on an example queue and exchange. Use this
 * as your guiding example for creating your own Dispatcher.
 *
 */
class ExampleSerializedAMQPDispatcher[T](factory: ConnectionFactory, host: String, port: Int) 
    extends AMQPDispatcher[T](factory, host, port) {
  override def configure(channel: Channel) {
    // Get the ticket.
    val ticket = channel.accessRequest("/data")
    // Set up the exchange and queue
    channel.exchangeDeclare(ticket, "mult", "direct")
    channel.queueDeclare(ticket, "mult_queue")
    channel.queueBind(ticket, "mult_queue", "mult", "routeroute")
    // Use the short version of the basicConsume method for convenience.
    channel.basicConsume(ticket, "mult_queue", false, new SerializedConsumer(channel, this))
  }
}

/**
 * Example class that accepts Strings coming in from the 
 * ExampleSerializedAMQPDispatcher.
 */ 
class ExampleStringAMQPListener {
  val params = new ConnectionParameters
  params.setUsername("guest")
  params.setPassword("guest")
  params.setVirtualHost("/")
  params.setRequestedHeartbeat(0)

  val factory = new ConnectionFactory(params)
  // thor.local is a machine on your network with rabbitmq listening on port 5672
  val amqp = new ExampleSerializedAMQPDispatcher[String](factory, "thor.local", 5672)
  amqp.start

  // Example Listener that just prints the String it receives.
  class StringListener extends Actor {
    def act = {
      react {
	case msg@AMQPMessage(contents: String) => println("received: " + msg); act
      }
    }
  }
  val stringListener = new StringListener()
  stringListener.start
  amqp ! AMQPAddListener(stringListener)
}
