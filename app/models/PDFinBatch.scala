package models

import java.io.ByteArrayOutputStream
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps
import org.xhtmlrenderer.pdf.ITextRenderer
import scala.util.Try
import scala.io.Source
import scala.xml.XML

/**
 * run Batch for PDF after Web app. is started,
 *  if system environment variable PDFBATCH is set;
 *  then the PDF's will be in directory PDF
 */
trait PDFinBatch // [Rdf <: RDF, DATASET]
    extends ResponseAnalysisInterface {
  Future { // wait a reasonable time that the server is started
    Thread.sleep(30000)
    val PDFBATCH = System.getenv("PDFBATCH")
    println("PDFBATCH=" + PDFBATCH)
    if( PDFBATCH != "no" ) {
      println("Starting Batch for PDF")
      Try {
        new File("PDF").mkdir
        // loop on users
        for (user_ <- User.listUsers) {
          implicit val user: User = user_
          println(s"PDFinBatch: User = $user_")
          val pdfContent = makePDFforOneUser()
          val file = user.email + ".pdf"
          val path = Paths.get("PDF", file)
          Files.write(path, pdfContent)
          println(s"Writen $path")
        }
      }
    }
  }

  def serverPort: String

  /**
   * pasted from trait ApplicationTrait;
   *  request is only used for request.host !!!
   */
  def makePDFforOneUser()(implicit user: User) = {
    val renderer = new ITextRenderer
    val buffer = new ByteArrayOutputStream
    implicit val host: String = "localhost:" + serverPort

    val report = views.html.report2(this)
    val reportAsString = report.toString.trim
    // DEBUG
    println(s"""PDFinBatch: makePDFforOneUser after views.html.report2() length ${reportAsString.length}
      ${reportAsString.substring(reportAsString.length - 200, reportAsString.length - 1)}""")
    val file = user.email + ".xml"
    val path = Paths.get("PDF", file)
    Files.write(path, reportAsString.getBytes())

    println("wrapInXML(reportAsString)" + wrapInXML(reportAsString).toString)
    renderer.setDocumentFromString(wrapInXML(reportAsString).toString)
    println(s"PDFinBatch: makePDFforOneUser after setDocumentFromString ")

    renderer.layout
    renderer.createPDF(buffer)
    buffer.toByteArray()
  }

  def wrapInXML(reportData: String) = {
    <html>
      <head>
        <style>
          @@font-face {{
				font-family: "DIN";
				src: url("/assets/fonts/DINPro-Regular.otf") format('opentype');
			}}
			body {{
			    font-family: "DIN","Helvetica Neue",Helvetica,Arial,sans-serif;
			    font-size: 14px;
			    line-height: 1.42857;
			    color: #333;
			}}
			h1, h2 {{
				color: #1C1862;
			}}
        </style>
      </head>
      <body>
        <h1>Évaluer et Sécuriser le capital immatériel</h1>
        <hr/>
        {
          // ugly hack because Scala does not parse string to NodeSeq's
          val node = scala.xml.XML.loadString("<xml>" + reportData + "</xml>")
          val list = node.child
          list
        }
      </body>
    </html>
  }
}