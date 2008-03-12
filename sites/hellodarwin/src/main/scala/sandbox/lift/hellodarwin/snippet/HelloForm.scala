package sandbox.lift.hellodarwin.snippet

import net.liftweb.http.S

class HelloForm {
  def who = <tt>{S.param("whoField").openOr("")}</tt>
}

