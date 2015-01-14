package controllers

import play.api._
import play.api.mvc._
import deductions.runtime.html.CreationForm
import Auth._

object Application extends Controller with Secured {
  
  def index = withAuth { username => implicit request =>
      Ok(views.html.index("Application"))
  }
  
  def form = withAuth { username => implicit request =>
      Ok(views.html.form(new CreationForm { actionURI = "/save" }.create("http://xmlns.com/foaf/0.1", "en").get))
  }
  
  def save = withAuth { username => implicit request =>
      Ok(views.html.index("Merci pour ce formulaire !"))
  }

}