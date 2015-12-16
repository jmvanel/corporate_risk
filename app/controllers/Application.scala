package controllers

import play.api._
import play.api.Play.current
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import deductions.runtime.html.{ CreationFormAlgo, TableViewModule }
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.jena.RDFCache
import deductions.runtime.services.FormSaver
import java.net.URLDecoder
import java.io.ByteArrayOutputStream
import org.jfree.chart.axis.CategoryLabelPositions
import org.jfree.chart.StandardChartTheme
import org.xhtmlrenderer.pdf.ITextRenderer
import scalax.chart.api._
import deductions.runtime.dataset.RDFStoreLocalProvider
import org.w3.banana.RDF
import org.w3.banana.jena.Jena
import com.hp.hpl.jena.query.Dataset
import com.typesafe.plugin._
import models.FormUserData
import models.UserDataTrait
import models.{
  User,
  UserCompanyInfo,
  UserData,
  UserVocab,
  //  ResponseAnalysis, 
  ContactInfo
}
import Auth._
import org.w3.banana.SparqlOpsModule
import org.w3.banana.RDFOpsModule
import deductions.runtime.semlogs.TimeSeries
import models.UserCompanyInfo
import deductions.runtime.html.TableViewModule
import scalax.chart.SpiderWebChart
import org.w3.banana.jena.JenaModule
import org.apache.log4j.Logger
import deductions.runtime.services.Configuration
import models.TimeSeriesFormGroups
import views.Charts
import models.ResponseAnalysisTrait
import models.ResponseAnalysisInterface

object Application extends Controller with Secured
    with JenaModule
    with TableViewModule[Jena, Dataset]
    with RDFStoreLocalJena1Provider
    with FormSaver[Jena, Dataset]
    //    with ResponseAnalysisTrait[Jena, Dataset]
    with ResponseAnalysisInterface
    with TimeSeriesFormGroups[Jena, Dataset]
    with Charts[Jena, Dataset]
    with Configuration {

  override val recordUserActions: Boolean = true
  override val addRDFS_label_comment = false

  addSaveListener(this) // for TimeSeries

  val logger: Logger = Logger.getRootLogger()
  lazy val tableView = this // new TableView {}
  val responseAnalysis = this // new ResponseAnalysis()

  //////// UI for user information ////////

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

  //////// UI for business forms ////////

  /** show a list of forms */
  def formgroup(groupUri: String) = withUser { implicit user =>
    implicit request =>
      val fgName = UserData.formGroupList.map(_.swap).get(groupUri).get
      val forms = UserData.getUserData(user, groupUri).map {
        case FormUserData(formUri, label) =>
          (formUri.getURI, label,
            responseAnalysis.responsesCount(user, formUri.getURI),
            responseAnalysis.fieldsCount(user, formUri.getURI)
          )
      }
      Ok(views.html.formgroup(forms, fgName))
  }

  /** edit given form, with values previously set by the user */
  def form(uri: String) = withUser { implicit user =>
    implicit request => {
      val formGroup = UserData.getFormGroup(user, uri)
      implicit val graph = rdfStore.r(dataset, { allNamedGraph }).get
      val form = tableView.htmlFormElemJustFields(uri, editable = true,
        graphURI = user.getURI().getURI(), formGroup = formGroup)
      val label = UserData.getFormLabel(uri)
      Ok(views.html.form(form, label, formGroup))
    }
  }

  /**
   * saves the values entered by the user,
   *  and redirect to next form in form group, if there is still one
   */
  def save = withUser { implicit user =>
    implicit request =>
      request.body match {
        case form: AnyContentAsFormUrlEncoded =>
          saveTriples(
            form.data.filterNot {
              case (key: String, _) => key.startsWith("SAVE")
            })
          form.data.getOrElse("uri", Seq()).headOption match {
            case Some(url) => {
              val nextform = if (form.data.contains("SAVE_previous")) {
                UserData.getPreviousForm(user, URLDecoder.decode(url, "utf-8"))
              } else if (form.data.contains("SAVE_next")) {
                UserData.getNextForm(user, URLDecoder.decode(url, "utf-8"))
              } else
                UserData.getNextForm(user, URLDecoder.decode(url, "utf-8"))

              nextform match {
                case Some(form) => Redirect(routes.Application.form(form.data.getURI))
                case None => Redirect(routes.Application.index.url)
              }
            }
            case _ => throw new IllegalArgumentException(form.asText.toString)
          }
        case _ => throw new IllegalArgumentException(request.body.asText.toString)
      }
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

  /**
   * compute Chart: chart type is "risk" or "capital"
   *  TODO: handle security
   */
  def chart(charttype: String, email: String) = Action {
    val content = computeChart(charttype, email)
    Ok(content.encodeAsPNG(320, 320)).withHeaders(CONTENT_TYPE -> "image/png")
  }

  def chart_old(charttype: String, email: String) = Action {
    val user = User.find(email)
    val transparent = new Color(0xFF, 0xFF, 0xFF, 0)
    //    implicit 
    val theme = new StandardChartTheme("JFree")
    theme.setChartBackgroundPaint(transparent)
    val content = charttype match {
      case "risk" => SpiderWebChart(responseAnalysis.getRiskEval(email).toVector)
      case "capital" => {
        val chart = BarChart(responseAnalysis.getCapitalEval(email).toVector)
        chart.plot.getDomainAxis().setCategoryLabelPositions(CategoryLabelPositions.UP_45)
        chart
      }
      case _ => throw new IllegalArgumentException("Please specify the kind of chart you want")
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
    mail.setRecipient(Play.current.configuration.getString("smtp.admin").get)
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

  //  type DataMatch = (String, String)
  def filterQuestionnaires(user: User, groupUri: String): (Seq[DataMatch] /*Good*/ , Seq[DataMatch] /*Good*/ ) = (Seq(), Seq())

}
