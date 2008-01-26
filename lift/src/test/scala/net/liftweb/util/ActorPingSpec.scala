package net.liftweb.util
import org.specs._
import org.specs.runner._
import scala.actors.Actor
import java.util.concurrent._

class ActorPingSpecTest extends JUnit3(ActorPingSpec)
object ActorPingSpecRunner extends ConsoleRunner(ActorPingSpec)
object ActorPingSpec extends Specification with PingedService with WaitFor {
  "The ActorPing object" should {
    "provide a schedule method to ping an actor regularly" in {
    //  waitFor(100.ms) {
    //    service.start
    //    ActorPing.schedule(service, Alive, 10)
    //  } 
    //  service.pinged must beTrue
    }
  }
}
trait WaitFor {
  implicit def intToDelay(i: Int) = Delay(i)
  case class Delay(i: Int) {
    var timeUnit = TimeUnit.MILLISECONDS
    def ms = {timeUnit = TimeUnit.MILLISECONDS; this}
    def s = {timeUnit = TimeUnit.SECONDS; this}
    def millis = if (timeUnit == TimeUnit.MILLISECONDS) i else i * 1000
  }
  def waitFor[T](delay: Delay)(action: => T): T = {
    waitFor(delay.millis)(action)
  }
  def waitFor[T](i: Int)(action: => T): T = {
    val result = action
    Thread.sleep(i)
    result
  }
}
trait PingedService {
  case object Alive
  val service = new Service
  class Service extends Actor {
    var pinged = false
    def act() {
      while (true) {
        receive {
          case Alive => {pinged = true; exit()}
        }
      }
    }
  }
}
