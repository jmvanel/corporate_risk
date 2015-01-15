package models
 
import java.util._
import play.api._
 
class User(var email: String, var password: String) {
  def checkPassword(password: String): Boolean = this.password == password
}

//User lookup
//object User {
//  def find(username: String):Option[User] =
//  	User("admin@example.com", "password")//users.filter(_.username == username).headOption
//}