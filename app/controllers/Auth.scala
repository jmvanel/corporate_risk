package controllers

import play.api._
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import models.User

/** TODO indiquer la source d'inspiration */
object Auth extends Controller {

  /** check user password in RDF database */
  val loginForm = Form(
    tuple("email" -> text, "password" -> text)
      verifying (
        "Invalid email or password",
        result => result match {
          case (email, password) => check(email, password)
        }))

  /** save user instance in RDF database */
  val registerForm = Form(
    tuple("email" -> text, "password" -> text, "confirmPassword" -> text)
      verifying (
        "Passwords do not match",
        result => result match {
          case (email, password, confirmPassword) => password == confirmPassword
        })
        verifying (
          "User already exists",
          result => result match {
            case (email, password, confirmPassword) =>
              val newUser = new User(email, password)
              newUser.save(newUser)
          }))

  def check(username: String, password: String): Boolean = {
    val user = new User(username, password)
    user.checkPassword()
  }

  /** page for login or signin */
  def login = Action { implicit request =>
    Ok(views.html.login(loginForm, registerForm))
  }

  /**
   * start a session after login if user Id & password are OK;
   * this is the action of form `loginForm`
   */
  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.login(formWithErrors, registerForm)),
      user => Redirect(routes.Application.index).withSession(Security.username -> user._1)
    )
  }

  /**
   * start a session after registering user Id & password;
   *  this is the action of form `registerForm`
   */
  def register = Action { implicit request =>
    registerForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.login(loginForm, formWithErrors)),
      user => Redirect(routes.Application.index).withSession(Security.username -> user._1)
    )
  }

  def logout = Action {
    Redirect(routes.Auth.login).withNewSession.flashing(
      "success" -> "You are now logged out."
    )
  }
}

/**
 * Trait for user/password secured controllers
 *  cf https://github.com/playframework/playframework/blob/master/framework/src/play/src/main/scala/play/api/mvc/Security.scala
 */
trait Secured {

  def username(request: RequestHeader) = request.session.get(Security.username)

  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Auth.login)

  /** Ensures the controller is only accessible to registered users */
  private def withAuth(f: => String => Request[AnyContent] => Result) = {
    Security.Authenticated(username, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
  }

  /** Ensures authentication and passes the user to the controller */
  def withUser(fun: User => Request[AnyContent] => Result) = withAuth { username =>
    implicit request =>
      User.find(username).map { user =>
        fun(user)(request)
      }.getOrElse(onUnauthorized(request))
  }
}