package models
 
import play.api._
import org.w3.banana._
import org.w3.banana.jena.JenaModule
import java.security.MessageDigest
 
trait RDFUser extends RDFModule with RDFOpsModule {
  
  import ops._
  
  val BizinnovPrexif = "http://bizinnov.com/"
  
  def checkPassword(password: String): Boolean = true
    //this.password == MessageDigest.getInstance("MD5").digest(password)
  
  //TODO: import Store
  //def makeRDFStore(file: String): Store
  
  def save = {
//    val triples = List(makeTriple(
//      makeUri(BizinnovPrexif + "user1"),
//      makeUri(BizinnovPrexif + "email"),
//      makeUri(BizinnovPrexif + this.email)
//    ),makeTriple(
//      makeUri(BizinnovPrexif + "user1"),
//      makeUri(BizinnovPrexif + "password"),
//      makeUri(BizinnovPrexif + MessageDigest.getInstance("MD5").digest(this.password))
//    ))
//    makeRDFStore("tmpGraphStoreDir").appendToGraph(makeUri("???"), Graph(triples))
  }
}

class User(var email: String, var password: String) extends RDFUser with JenaModule

//User lookup
object User {
  val users = List(new User("admin", "1234"), 
      new User("user", "password"))
  def find(email: String):Option[User] =
  	users.filter(_.email == email).headOption
}