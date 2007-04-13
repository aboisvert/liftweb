package net.liftweb.example.snippet

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */
    
import net.liftweb.example.model._
import scala.xml.{NodeSeq, Text, Group}
import net.liftweb.http.S
import net.liftweb.http.S._
import net.liftweb.util.Helpers._

class Misc {
  def users: NodeSeq = 
    <tr>{User.htmlHeaders}<th>Edit</th><th>Delete</th></tr> ::
    User.findAll.flatMap(u => <tr>{u.htmlLine}
    <td><a href={"/simple/edit?id="+u.id}>Edit</a></td>
    <td><a href={"/simple/delete?id="+u.id}>Delete</a></td>
    </tr>)
    
    def confirmDelete(xhtml: Group): NodeSeq = {      
      (for (val id <- param("id");
           val user <- User.find(id)) yield {
        def deleteUser(p: String) {
          notice("User "+(user.firstName+" "+user.lastName)+" deleted")
          user.delete_!
          redirectTo("/simple/index.html")
        }

	bind("xmp", xhtml, "username" -> (user.firstName+" "+user.lastName),
            "delete" -> submit(deleteUser, Val("Delete")))
            
      }) getOrElse {error("User not found"); redirectTo("/simple/index.html")}
    }

      def edit(xhtml: Group): NodeSeq = {
        val invokedAs = S.invokedAs
        
        (for (val id <- param("id");
              val user <- User.find(id)) yield {
          def runContext(xhtml: NodeSeq): NodeSeq = {
          def saveUser(ignore: List[String]): boolean = {
            user.validate match {
              case Nil => user.save; redirectTo("/simple/index.html")
              case x => error(x) ; mapSnippet(invokedAs, runContext)
            }
            true
          }
          user.toForm(saveUser) ++ <tr><td><a href="/simple/index.html">Cancel</a></td><td><input type="submit" value="Save"/></td></tr>
          }
          
          runContext(xhtml)
        }) getOrElse {error("User not found"); redirectTo("/simple/index.html")}
      }
}
