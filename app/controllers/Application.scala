package controllers

import play.api._
import play.api.mvc._
import deductions.runtime.html.{ CreationForm, TableView }
import Auth._

import models.UserData

object Application extends Controller with Secured {
  lazy val tableView = new TableView {}

  def index = withUser { user =>
    implicit request =>
      Ok(views.html.index(UserData.getUserData(user).map(_.getURI)))
  }

  /** edit given URI */
  def form(uri: String) = withAuth { username =>
    implicit request =>
      println("editURI: " + request)
      Ok(views.html.form(tableView.htmlForm(uri, editable = true
      //        lang = chooseLanguage(request)
      )))

  }

  /** create new instance of given class (unused) */
  def create(url: String) = withAuth { username =>
    implicit request =>
      Ok(views.html.form(new CreationForm { actionURI = "/save" }.create(url, "en").get))
  }

  /** */
  def save(url: String) = withAuth { username =>
    implicit request =>
      val tableView = new TableView {}
      Ok(views.html.report(tableView.htmlForm(url, "", "").get))
  }

}