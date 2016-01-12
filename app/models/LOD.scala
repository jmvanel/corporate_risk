package models

import deductions.runtime.services.SPARQLHelpers
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.apache.log4j.Logger
import org.w3.banana.RDF
import org.w3.banana.TryW
import org.w3.banana.io.RDFWriter
import org.w3.banana.io.Turtle

trait LOD[Rdf <: RDF, DATASET]
    extends SPARQLHelpers[Rdf, DATASET] {

  import ops._

  def getNAFLabel(number: String, lang: String = "fr"): String = {
    val notation = if (!number.contains("."))
      number.substring(0, 2) + "." + number.substring(2)
    else
      number
    println(s">>>> getNAFLabel notation $notation")
    val query =
      s"""
         |prefix skos: <http://www.w3.org/2004/02/skos/core#>
         |SELECT DISTINCT ?LABEL WHERE {
         |  graph ?g {
         |    ?CONCEPT skos:notation "${notation}" ;
         |             skos:prefLabel ?LABEL .
         |  }
         |}""".stripMargin
    val res = sparqlSelectQueryVariables(query, Seq("LABEL"))
    val node = res.flatten.headOption.getOrElse(Literal(""))
    val valueFromTDB = foldNode(node)(_ => "", _ => "", l => fromLiteral(l)._1)
    
    if( valueFromTDB != "" )
      valueFromTDB
    else
      s"Le code NAF $number ($notation) n'existe pas."
  }
}