package controllers

import play.api._
import play.api.mvc._
import deductions.runtime.html.{ CreationForm, TableView }
import Auth._

import models.UserData

object Application extends Controller with Secured {

  def index = withUser { implicit user =>
    implicit request =>
      Ok(views.html.index(UserData.getUserData(user).map(_.getURI)))
  }

  def form(url: String) = withUser { implicit user =>
    implicit request =>
      Ok(views.html.form(new CreationForm { actionURI = "/save" }.create(url).get))
  }

  def save = withUser { implicit user =>
    implicit request =>
      val tableView = new TableView {}
      Ok(views.html.report(tableView.htmlForm("http://xmlns.com/foaf/0.1", "", "").get))
  }

}