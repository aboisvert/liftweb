package net.liftweb.record

import field._
import net.liftweb.http.{S, FieldError}
import net.liftweb.util.{Can, Full, Empty}
import scala.xml._

object Test {
  def main(args : Array[String]) : Unit = {

   val rec = MyRecordMeta.createRecord.firstName("John").lastName("asd retq").age(10).mail("sdfert")

   val template =
     <div>
	   <div>
	     <div><lift:field_label name="firstName"/></div>
	     <div><lift:field name="firstName"/></div>
	     <div><lift:field_msg name="firstName"/></div>
       </div>
	   <div>
	     <div><lift:field_label name="lastName"/></div>
	     <div><lift:field name="lastName"/></div>
	     <div><lift:field_msg name="lastName"/></div>
       </div>
     </div> ++
     <div>
	   <div >
	     <div><lift:field_label name="age"/></div>
	     <div><lift:field name="age"/></div>
	     <div><lift:field_msg name="age"/></div>
       </div>
	   <div >
	     <div><lift:field_label name="agree"/></div>
	     <div><lift:field name="agree"/></div>
	     <div><lift:field_msg name="agree"/></div>
       </div>
      </div>
     S.initIfUninitted(null){
     println(rec.toForm(template){(r:MyRecord) => println(r)});
   }

   println(rec.lastName)

   val valid : List[FieldError] = rec.validate
   println("validate " + valid)
  }
}

object MyRecordMeta extends MyRecord with DBMetaRecord[MyRecord] {
  override def mutable_? = false
}

class MyRecord extends DBRecord[MyRecord] {

  def meta = MyRecordMeta

  object firstName extends DBStringField(this, "Marius") {
    override def tabIndex = 1
    override def validators = notEmpty _ :: Nil
  }

  object lastName extends DBStringField(this, "Danciu") {
    override def validators = notEmpty _ :: noSpaces _ :: Nil
    override def tabIndex = 3
  }

  object age extends DBIntField(this) {
   override def validators = ageControl _ :: Nil
   override def tabIndex = 2
  }

  object hook extends LifecycleCallbacks {
    override def beforeValidation {
      println("Before validation")
    }
    override def afterValidation {
      println("After validation")
    }
  }

  object agree extends DBBooleanField(this)

  object mail extends DBEmailField(this, "marius.danciu@nokia.com")

  def ageControl(age: Int) : Can[Node] = if (firstName.value == "John" && age < 25) {
    Full(Text("Age can not be set if your name is John and you're under 25 years old"))
  } else {
    Empty
  }

  def notEmpty(s: String) : Can[Node] =
    if (s.length == 0)
      Full(Text("Field can not be empty"))
    else
      Empty

  def noSpaces(s: String) : Can[Node] =
    if (s.indexOf(" ") >= 0)
      Full(Text("Field can not contain spaces"))
    else
      Empty

}
