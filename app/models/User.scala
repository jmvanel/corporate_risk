package models
 
import java.util._
import play.api._
 
class User(var email: String, var password: String) {
 def checkPassword(password: String): Boolean = this.password == password
}
/*
//User lookup
object User {
  def find(username: String):Option[User] =
  	User("admin@example.com", "password")//users.filter(_.username == username).headOption
}

object AuthUtils {
  def parseUserFromCookie(implicit request: RequestHeader) = 
    request.session.get("username").flatMap(username => User.find(username))
 
  def LoginRequired(f: User => Result) = Action {
    val user:Option[User] = User.find(username).filter(_.checkPassword(password))
    user match {
      case Some(user) => f(user)
      case None => Redirect(routes.Application.index)
    }
  }
  
  def authenticateFromRequest(implicit request:RequestHeader) = {
    val query = request.queryString.map { case (k, v) => k -> v.mkString }
    (query get ("username"), query get ("password")) match {
      case (Some(u), Some(p)) => User.find(u).filter(user => user.checkPassword(p))
      case _ => None
    }
  }
}*/