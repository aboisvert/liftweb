import javax.persistence.{EntityManager,Persistence}
import scala.collection.jcl.{BufferWrapper,SetWrapper}
import junit.framework._;
import Assert._;
import net.liftweb.util.{Can,Full,Empty}
import com.foo.jpaweb.model.JPA._
import com.foo.jpaweb.model._

class AuthorTest extends TestCase {
  
  def testOK = {
    
    val authors = Model.createNamedQuery[Author]("findAllAuthors").getResultList()
    
    authors.foreach(author => {
        author.books.foreach(book => assertNotNull("bookTitle",book.title))
      })
    
    val author2 = Model.find(classOf[Author], 2L)
    assertNotNull("author2", author2)

    val author50 = Model.find(classOf[Author], 50L)
    assertNull("author50Can", author50)
    
    val authorMT =
    Model.createQuery[Author]("from Author where name = :name")
        .setParameter("name", "Jules Verne").getSingleResult()
    assertEquals("authorMTCan", author2.name, authorMT.name)

  }

}
