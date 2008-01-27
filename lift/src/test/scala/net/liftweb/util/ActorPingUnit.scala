package net.liftweb.util
import org.specs._
import org.specs.runner._
import scala.actors.Actor
import java.util.concurrent._

class ActorPingUnitTest extends JUnit3(ActorPingUnit)
object ActorPingUnitRunner extends ConsoleRunner(ActorPingUnit)
object ActorPingUnit extends Specification with PingedService with WaitFor {
  def pingService = {
    service.start
    ActorPing.schedule(service, Alive, 10)
    waitFor(100.ms)
    service.pinged must beTrue
  }
  "The ActorPing object" can {  
    "be restarted twice" in {
      ActorPing.restart
      ActorPing.restart
			pingService      
    }
    "be shutdown and restarted" in {
      ActorPing.shutdown
      ActorPing.restart
      pingService      
    }
    "be shutdown twice" in {
      ActorPing.shutdown
      ActorPing.shutdown
      pingService must throwAn(ActorPingException("", null))
    }
  }
}
