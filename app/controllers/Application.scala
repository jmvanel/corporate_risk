package controllers

import play.api._
import play.api.Play.current
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import deductions.runtime.html.{ CreationForm, TableView }
import deductions.runtime.services.FormSaver
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.sparql_cache.RDFCache
import java.net.URLDecoder
import java.io.ByteArrayOutputStream
import org.xhtmlrenderer.pdf.ITextRenderer
import scalax.chart.api._
import deductions.runtime.dataset.RDFStoreLocalProvider
import org.w3.banana.RDF
import org.w3.banana.jena.Jena
import com.hp.hpl.jena.query.Dataset
import com.typesafe.plugin._
import models.FormUserData
import models.UserDataTrait
import models.{ User, UserCompanyInfo, UserData, UserVocab, ResponseAnalysis }
import Auth._
import org.w3.banana.SparqlOpsModule
import org.w3.banana.RDFOpsModule
import deductions.runtime.abstract_syntax.InstanceLabelsInference2
import models.UserCompanyInfo

object Application extends ApplicationTrait[Jena, Dataset]
  with RDFStoreLocalJena1Provider

/** Class for contact information for email and phone call request */
case class ContactInfo(name: String, job: Option[String], city: Option[String], phone: Option[String], email: Option[String], message: String)

trait ApplicationTrait[Rdf <: RDF, DATASET] extends Controller with Secured
    with UserDataTrait[Rdf, DATASET]
    with InstanceLabelsInference2[Rdf] {

  lazy val tableView = new TableView {}
  val responseAnalysis = new ResponseAnalysis()
  import ops._

  /** User company information form for the index page */
  val userInfoForm = Form(
    mapping("department" -> optional(text),
      "naf" -> optional(text),
      "year" -> optional(text),
      "isGroup" -> optional(text)
    )(UserCompanyInfo.apply)(UserCompanyInfo.unapply)
  )

  /** Contact form */
  val contactForm = Form(
    mapping(
      "name" -> text,
      "job" -> optional(text),
      "city" -> optional(text),
      "phone" -> optional(text),
      "email" -> optional(text),
      "message" -> text
    )(ContactInfo.apply)(ContactInfo.unapply)
  )

  /** Shows the index page with the info form */
  def index = withUser { implicit user =>
    implicit request =>
      val form = userInfoForm.bind(user.getInfo(user).getOrElse(new UserCompanyInfo).getMap)
      Ok(views.html.index(form))
  }

  /** Saves the info form for the user */
  def saveinfo = withUser { implicit user =>
    implicit request => {
      userInfoForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.index(formWithErrors)),
        userInfo => {
          user.saveInfo(user, userInfo)
          Redirect(routes.Application.formgroup(UserData.formGroupList.get("Pré-diagnostic").get))
        }
      )
    }
  }

  /** show a list of forms */
  def formgroup(groupUri: String) = withUser { implicit user =>
    implicit request =>
      val fgName = UserData.formGroupList.map(_.swap).get(groupUri).get
      val forms = getUserData(user, URI(groupUri)).map {
        case FormUserData(formUri, label) =>
          (fromUri(formUri), label,
            responseAnalysis.responsesCount(user, fromUri(formUri))
          )
      }

      Ok(views.html.formgroup(forms, fgName))
  }

  /** edit given form, with values previously set by the user */
  def form(uri: String) = withUser { implicit user =>
    implicit request =>
      val label = rdfStore.r(dataset, {
        implicit val graph = allNamedGraph
        instanceLabel(URI(uri))
      }).get
      Ok {
        views.html.form(
          tableView.htmlFormElem(uri, editable = true, graphURI = user.getURI().toString()),
          label)
      }
  }

  /** saves the values entered by the user */
  def save = withUser { implicit user =>
    implicit request =>
      val uri = request.body match {
        case form: AnyContentAsFormUrlEncoded =>
          deductions.runtime.services.FormSaverObject.saveTriples(form.data)
          form.data.getOrElse("uri", Seq()).headOption match {
            case Some(url) => URLDecoder.decode(url, "utf-8")
            case _ => throw new IllegalArgumentException
          }
        case _ => throw new IllegalArgumentException
      }
      Redirect(routes.Application.index.url);
  }

  /** shows the report for the given user, as a html preview */
  def report = withUser { implicit user =>
    implicit request =>
      Ok(views.html.webreport(responseAnalysis))
  }

  /** downloads a pdf version of the report */
  def exportPDF = withUser { implicit user =>
    implicit request =>
      val renderer = new ITextRenderer
      val buffer = new ByteArrayOutputStream
      renderer.setDocumentFromString(views.html.pdfreport(responseAnalysis).toString)
      renderer.layout
      renderer.createPDF(buffer)
      Ok(buffer.toByteArray()).withHeaders(CONTENT_TYPE -> "application/pdf")
  }

  //TODO: handle security
  def chart(charttype: String, email: String) = Action {
    val user = User.find(email)
    val content = charttype match {
      case "risk" => SpiderWebChart(responseAnalysis.getRiskEval(email).toVector)
      case "capital" => BarChart(responseAnalysis.getCapitalEval(email).toVector)
      // TODO: les vrais chiffres:
      case "pie" => PieChart(Vector(("Sécurité", 4), ("Fiabilité", 1), ("Gouvernance", 4), ("Vitesse", 2), ("Solidité", 3)))
      case _ => BarChart(Vector(("default case", 1)))
    }
    Ok(content.encodeAsPNG(320, 320)).withHeaders(CONTENT_TYPE -> "image/png")
  }

  def contact() = Action { implicit request =>
    val user = request.session.get(Security.username).map { email => User.find(email).get }
    Ok(views.html.contact(user, contactForm))
  }

  def sendEmail(sender: String, subject: String, body: String) = {
    val mail = use[MailerPlugin].email
    mail.setSubject(subject)
    mail.setRecipient(Play.current.configuration.getString("smtp.admin"))
    mail.setFrom(sender)
    mail.send(body)
  }

  def contactRequest() = Action { implicit request =>
    val user = request.session.get(Security.username).map { email => User.find(email).get }
    contactForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.contact(user, formWithErrors)),
      contactInfo => {
        contactInfo.email match {
          case Some(emailAdress) =>
            sendEmail(emailAdress, "Message de V.S.I", contactInfo.message)
            Ok(views.html.contact(user, contactForm, "L'e-mail a bien été envoyé."))
          case None =>
            contactInfo.phone match {
              case Some(phoneNumber) =>
                sendEmail("", "Demande de rappel de V.S.I", phoneNumber)
                Ok(views.html.contact(user, contactForm, "La demande a été enregistrée. Nous vous contacterons prochainement."))
              case None =>
                Ok(views.html.contact(user, contactForm, "Merci d'entrer une information de contact"))
            }
        }
      })
  }

  def info() = Action { implicit request =>
    val user = request.session.get(Security.username).map { email => User.find(email).get }
    Ok(views.html.info(user))
  }
}
