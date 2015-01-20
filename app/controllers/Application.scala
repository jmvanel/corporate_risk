package controllers

import play.api._
import play.api.mvc._
import deductions.runtime.html.{ CreationForm, TableView }
import Auth._

object Application extends Controller with Secured {

  def index = withAuth { username =>
    implicit request =>
      Ok(views.html.index("Tableau de bord"))
  }

  def form(url: String) = withAuth { username =>
    implicit request =>
      Ok(views.html.form(new CreationForm { actionURI = "/save" }.create(url, "en").get))
  }

  def save = withAuth { username =>
    implicit request =>
      val tableView = new TableView {}
      Ok(views.html.report(tableView.htmlForm("http://xmlns.com/foaf/0.1", "", "").get))
  }

}