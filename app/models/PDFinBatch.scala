package models

import java.io.ByteArrayOutputStream
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps

import org.xhtmlrenderer.pdf.ITextRenderer

/**
 * run Batch for PDF after Web app. is started,
 *  if system environment variable PDFBATCH is set;
 *  then the PDF's will be in directory PDF
 */
trait PDFinBatch // [Rdf <: RDF, DATASET]
    extends ResponseAnalysisInterface {
  Future {
    // wait a reasonable time that the server is started
    Thread.sleep(30000)
    if (System.getenv("PDFBATCH") != null) {
      println("Starting Batch for PDF")
      new File("PDF").mkdir
      // loop on users
      for (user_ <- User.listUsers) {
        implicit val user: User = user_
        val pdfContent = makePDFforOneUser()
        val file = user.email + ".pdf"
        val path = Paths.get("PDF", file)
        Files.write(path, pdfContent)
        println(s"Writen $path")
      }
    }
  }

  /**
   * pasted from trait ApplicationTrait;
   *  request is only used for request.host !!!
   */
  def makePDFforOneUser()(implicit user: User) = {
    val renderer = new ITextRenderer
    val buffer = new ByteArrayOutputStream
    implicit val host: String = "localhost"
    renderer.setDocumentFromString(views.html.report2(this).toString)
    renderer.layout
    renderer.createPDF(buffer)
    buffer.toByteArray()
  }
}