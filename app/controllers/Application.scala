package controllers

import java.io.ByteArrayOutputStream
import java.net.URLDecoder
import org.apache.log4j.Logger
import org.w3.banana.jena.Jena
import org.w3.banana.jena.JenaModule
import org.xhtmlrenderer.pdf.ITextRenderer
import com.hp.hpl.jena.query.Dataset
import com.typesafe.plugin.MailerPlugin
import com.typesafe.plugin.use
import deductions.runtime.html.CSS
import deductions.runtime.html.TableViewModule
import deductions.runtime.jena.ImplementationSettings
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.services.DefaultConfiguration
import deductions.runtime.services.FormSaver
import models.ContactInfo
import models.FormUserData
import models.FormsGroupsData1
import models.RDFUser
import models.ReportGenerationTrait
import models.ResponseAnalysisInterface
import models.TimeSeriesFormGroups
import models.User
import models.UserCompanyInfo
import models.UserDataTrait
import play.api.Play
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.data.Forms.optional
import play.api.data.Forms.text
import play.api.mvc.Action
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.mvc.Controller
import play.api.mvc.Security
import scalax.chart.api.ChartPNGExporter
import views.Charts
import models.LOD
import models.PDFinBatch
import deductions.runtime.services.DashboardHistoryUserActions
import views.History
import scala.util.Try
import java.nio.file.Path
import play.api.libs.iteratee.Enumerator
import play.api.mvc.Result
import play.api.mvc.ResponseHeader
import scala.util.Success
import scala.util.Failure

object Application extends ApplicationTrait
  with RDFUser[Jena, ImplementationSettings.DATASET]

trait ApplicationTrait
    extends Controller with Secured with JenaModule
    with DefaultConfiguration
    with TableViewModule[Jena, Dataset]
    with RDFStoreLocalJena1Provider
    with FormSaver[Jena, Dataset]
    with ResponseAnalysisInterface
    with TimeSeriesFormGroups[Jena, Dataset]
    with Charts[Jena, Dataset]
    with UserDataTrait[Jena, Dataset]
    with RDFUser[Jena, Dataset]
    with ReportGenerationTrait[Jena, Dataset]
    with FormsGroupsData1[Jena]
    with LOD[Jena, Dataset]
    with PDFinBatch
    with DashboardHistoryUserActions[Jena, Dataset] {

  // override defaults from semantic_forms:
  override val recordUserActions: Boolean = true
  override val addRDFS_label_comment = false
  override val showRDFtype = false
  override val showPlusButtons = false
  override val inlineJavascriptInForm = false
  override val displayTechnicalSemWebDetails = false
  override val css: CSS = new CSS {
    override val cssRules = ""
    /*
      .form-row{ display: table-row; }
      .form-cell{ display: table-cell; }
      .form-input{ display: table-cell; width: 500px; }
      .form-value{ display: table-cell; width: 500px; }
      .button-add{ width: 25px; }
      .form-label{ display: table-cell; width: 160px; }
     */
    override lazy val cssClasses = CSSClasses()
  }

  /** TODO pasted from semantic_forms */
  override def serverPort = {
    val port = Play.current.configuration.
      getString("http.port")
    port match {
      case Some(p) =>
        println("Running on port " + p)
        p
      case _ =>
        println("Retrieving default port from config.")
        super.serverPort
    }
  }

  addSaveListener(this) // for TimeSeries

  val logger: Logger = Logger.getRootLogger()
  lazy val tableView = this // new TableView {}
  val responseAnalysis = this // new ResponseAnalysis()
  val userData = this

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
      val form = userInfoForm.bind(getCompanyInfo(user).getOrElse(new UserCompanyInfo).getMap)
      Ok(views.html.index(form))
  }

  /** Saves the info form for the user */
  def saveinfo = withUser { implicit user =>
    implicit request => {
      userInfoForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.index(formWithErrors)),
        userInfo => {
          saveInfo(user, userInfo)
          Redirect(routes.Application.formgroup(userData.formGroupList(Some(user)).
            get("Pré-diagnostic").get.get))
        }
      )
    }
  }

  //////// UI for business forms ////////

  /** show a list of forms */
  def formgroup(groupUri: String) = withUser { implicit user =>
    implicit request =>
      val fgName = userData.formGroupList(Some(user)).map(_.swap).get(Some(groupUri)).get
      val forms = userData.getUserData(user, groupUri).map {
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
      val formGroup = userData.getFormGroup(user, uri)
      implicit val graph = rdfStore.r(dataset, { allNamedGraph }).get
      val form = tableView.htmlFormElemJustFields(uri, editable = true,
        graphURI = user.getURI().getURI(), formGroup = formGroup)
      val label = userData.getFormLabel(uri)
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
          implicit val userURI: String = user.getURI().toString()
          saveTriples(
            form.data.filterNot {
              case (key: String, _) => key.startsWith("SAVE")
            })
          form.data.getOrElse("uri", Seq()).headOption match {
            case Some(url) => {
              val nextform = if (form.data.contains("SAVE_previous")) {
                userData.getPreviousForm(user, URLDecoder.decode(url, "utf-8"))
              } else if (form.data.contains("SAVE_next")) {
                userData.getNextForm(user, URLDecoder.decode(url, "utf-8"))
              } else
                userData.getNextForm(user, URLDecoder.decode(url, "utf-8"))

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

  def exportAllPDF = withUser { implicit user =>
    implicit request =>
      val zipFileTry: Try[Path] = makeZip
      zipFileTry match {
        case Success(path) =>
          val file = path.toFile()
          import scala.concurrent.ExecutionContext.Implicits.global
          val fileContent: Enumerator[Array[Byte]] = Enumerator.fromFile(file)
          Result(
            header = ResponseHeader(200, Map(
              CONTENT_LENGTH -> file.length.toString,
              CONTENT_TYPE -> "application/zip",
              CONTENT_DISPOSITION -> ("attachment; filename=" + file.getName)
            )),
            // content-disposition", "attachment; filename=
            body = fileContent)
        case Failure(e) =>
          InternalServerError("A server error occurred: " + e.getLocalizedMessage)
      }
  }

  /**
   * compute Chart: chart type is "risk" or "capital"
   *  TODO: handle security
   */
  def chart(charttype: String, email: String) = Action {
    val content = computeChart(charttype, email)
    Ok(content.encodeAsPNG(640, 320)).withHeaders(CONTENT_TYPE -> "image/png")
  }

  def history(email: String, fg: Int) =
    withUser { implicit user =>
      implicit request => {
        /* how to send back several images in a single service */
        val allXYChart = computeAllXYChart(user.email)
        if (!allXYChart.isEmpty) {
          val content = allXYChart toIndexedSeq (fg)
          Ok(content.encodeAsPNG(320, 320)).withHeaders(CONTENT_TYPE -> "image/png")
        } else {
          Ok("Pas encore d'historique à afficher.")
        }
      }
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
                sendEmail("", "Demande de rappel de E.S.I", phoneNumber)
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

  /** make History of User Actions */
  def makeHistoryUserActionsAction(implicit userURI: String) =
    Action { implicit request =>
      val history =
        makeTableHistoryUserActions(lang = "fr")
      Ok(new History { val content = history }.page)
        .as("text/html; charset=utf-8")
    }

}
