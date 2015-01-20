package controllers

import play.api._
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import models.User

/** TODO indiquer la source d'inspiration */
object Auth extends Controller {

  val loginForm = Form(
    tuple("email" -> text, "password" -> text) verifying
      ("Invalid email or password", result => result match {
        case (email, password) => check(email, password)
      })
  )
  val registerForm = Form(
    tuple("email" -> text, "password" -> text, "confirmPassword" -> text) verifying
      ("Passwords do not match", result => result match {
        case (email, password, confirmPassword) => password == confirmPassword
      }) verifying
      ("User already exists", result => result match {
        case (email, password, confirmPassword) =>
          val newUser = new User(email, password)
          newUser.save(newUser)
      })
  )

  def check(username: String, password: String) = {
    val user = new User(username, password)
    user.checkPassword(user)
  }

  def login = Action { implicit request =>
    Ok(views.html.login(loginForm, registerForm))
  }

  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.login(formWithErrors, registerForm)),
      user => Redirect(routes.Application.index).withSession(Security.username -> user._1)
    )
  }

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

/** Trait for user/password secured controllers */
trait Secured {

  def username(request: RequestHeader) = request.session.get(Security.username)

  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Auth.login)

  /** Ensures the controller is only accessible to registered users */
  def withAuth(f: => String => Request[AnyContent] => Result) = {
    Security.Authenticated(username, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
  }

  /** Ensures authentication and passes the user to the controller */
  def withUser(f: User => Request[AnyContent] => Result) = withAuth { username =>
    implicit request =>
      User.find(username).map { user =>
        f(user)(request)
      }.getOrElse(onUnauthorized(request))
  }
}