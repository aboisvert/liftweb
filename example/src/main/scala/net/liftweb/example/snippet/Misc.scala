package net.liftweb.example.snippet

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.example.model._
import scala.xml.{NodeSeq, Text, Group}
import net.liftweb.http.S
import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.util.Helpers._

class Misc {
  /**
   * Get the XHTML containing a list of users
   */
  def users: NodeSeq = {
    // the header
    <tr>{User.htmlHeaders}<th>Edit</th><th>Delete</th></tr> ::
  // get and display each of the users
  User.findAll(OrderBy(User.id, true)).flatMap(u => <tr>{u.htmlLine} 
		       <td><a href={"/simple/edit?id="+u.id}>Edit</a></td>
		       <td><a href={"/simple/delete?id="+u.id}>Delete</a></td>
		       </tr>)
  }
   
  /**
   * Confirm deleting a user
   */
  def confirmDelete(xhtml: Group): NodeSeq = {      
    (for (id <- param("id"); // get the ID
          user <- User.find(id)) // find the user
     yield { 
       def deleteUser(p: String) {
         notice("User "+(user.firstName+" "+user.lastName)+" deleted")
         user.delete_!
         redirectTo("/simple/index.html")
       }
       
       // bind the incoming XHTML to a "delete" button.
       // when the delete button is pressed, call the "deleteUser"
       // function (which is a closure and bound the "user" object
       // in the current content)
       bind("xmp", xhtml, "username" -> (user.firstName+" "+user.lastName),
	    "delete" -> submit("Delete", deleteUser))
       
       // if the was no ID or the user couldn't be found,
       // display an error and redirect
     }) openOr {error("User not found"); redirectTo("/simple/index.html")}
   }

  /**
   * Edit a user
   */
  def edit(xhtml: Group): NodeSeq = {
    // save the name of this snippet
    val invokedAs = S.invokedAs
    
    
    (for (id <- param("id"); // get the id
          user <- User.find(id)) // find the user
     yield {
       // define a function that will be called to deal
       // with editing the user
       def runContext(xhtml: NodeSeq): NodeSeq = {

	 // called when the form is submitted
         def saveUser(ignore: List[String]): boolean = {
	   user.validate match {
	     // no validation errors, save the user, and go
	     // back to the "list" page
	     case Nil => user.save; redirectTo("/simple/index.html")

	     // oops... validation errors
	     // display them and tell "S"
	     // that when the page processes the "misc:edit"
	     // snippet to call the "runContext" function rather
	     // than instantiating a new "Misc" instance and calling
	     // the "edit" method.  The big win here is that
	     // we keep the "user" instance that's got the right
	     // state based on the prior form submissions
	     case x => error(x) ; mapSnippet(invokedAs, runContext)
	   }
	   true
         }

	 // get the form data for the user and when the form
	 // is submitted, call the passed function.
	 // That means, when the user submits the form,
	 // the fields that were typed into will be populated into
	 // "user" and "saveUser" will be called.  The
	 // form fields are bound to the model's fields by this
	 // call.
         user.toForm(saveUser) ++ <tr>
           <td><a href="/simple/index.html">Cancel</a></td>
           <td><input type="submit" value="Save"/></td>
         </tr>
       }
       
       // call the function
       // okay, you ask yourself "why define a function and call it?
       // why not just execute the code?"  It's like this, the function
       // is bound to the value of "user".  This means you can pass
       // the function back to "S" (the current execution environment)
       // such that the same function (with the same bound variables)
       // will be used for all calls until the user is saved.
       runContext(xhtml)
       
       // bail out if the ID is not supplied or the user's not found
     }) openOr {error("User not found"); redirectTo("/simple/index.html")}
  }
}
