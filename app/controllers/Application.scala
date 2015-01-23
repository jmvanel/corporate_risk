package controllers

import play.api._
import play.api.mvc._
import deductions.runtime.html.{ CreationForm, TableView }
import deductions.runtime.services.FormSaver
import deductions.runtime.sparql_cache.RDFCache
import java.net.URLDecoder

import models.UserData
import Auth._

object Application extends Controller with Secured with RDFCache {

  def index = withUser { implicit user =>
    implicit request =>
      Ok(views.html.index(UserData.getUserData(user).map(_.getURI)))
  }

  def form(url: String) = withUser { implicit user =>
    implicit request =>
      Ok(views.html.form(new CreationForm { actionURI = routes.Application.save.url }.create(url).get))
  }

  def save = withUser { implicit user =>
    implicit request =>
      val uri = request.body match {
        case form: AnyContentAsFormUrlEncoded =>
          new FormSaver().saveTriples(form.data)
          form.data.getOrElse("uri", Seq()).headOption match {
            case Some(url1) => URLDecoder.decode(url1, "utf-8")
            case _ => throw new IllegalArgumentException
          }
        case _ => throw new IllegalArgumentException
      }

      Ok(views.html.report(new TableView {}.htmlForm(uri).get))
  }

}