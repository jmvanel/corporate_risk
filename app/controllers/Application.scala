package controllers

import play.api._
import play.api.mvc._
import deductions.runtime.html.CreationForm

object Application extends Controller {

  def index = Action {
      Ok(views.html.index("Application"))
  }
  
  def form = Action {
      Ok(views.html.form(new CreationForm { actionURI = "/save" }.create("http://xmlns.com/foaf/0.1", "en").get))
  }
  
  def save = Action {
      Ok(views.html.index("Merci pour ce formulaire !"))
  }

}