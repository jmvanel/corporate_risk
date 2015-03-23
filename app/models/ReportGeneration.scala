package models

import scala.xml.Elem
import org.w3.banana.RDF
import org.w3.banana.SparqlGraphModule
import org.w3.banana.SparqlOpsModule
import org.w3.banana.XSDPrefix
import org.w3.banana.diesel._
import org.w3.banana.jena.Jena
import com.hp.hpl.jena.query.Dataset
import deductions.runtime.abstract_syntax.InstanceLabelsInference2
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.dataset.RDFStoreLocalProvider
import org.apache.log4j.Logger
import java.net.URLEncoder

/**
 * (textual) Report Generation :
 *  see "Note on the data model" in README.md
 */
class ReportGeneration extends RDFStoreLocalJena1Provider with ReportGenerationTrait[Jena, Dataset]

trait ReportGenerationTrait[Rdf <: RDF, DATASET]
    extends ResponseAnalysisTrait[Rdf, DATASET] {

  import ops._
  import rdfStore.transactorSyntax._
  import rdfStore.graphStoreSyntax._
  import rdfStore.sparqlEngineSyntax._

  type DataMatch = (String, Rdf#URI)

  def sentencesGoodBadPoints(user: User): Seq[Elem] = {
    filterGoodQuestionnaires(user).map {
      dataMatch => <p> Un point favorable est <a href={ "/form?url=" + urlEncode(dataMatch._2) }> { dataMatch._1 } </a>.</p>
    } ++
      Seq(<hr/>) ++
      filterBadQuestionnaires(user).map {
        dataMatch => <p> Un point à améliorer est <a href={ "/form?url" + urlEncode(dataMatch._2) }> { dataMatch._1 } </a>.</p>
      }
  }

  def filterGoodQuestionnaires(user: User): Seq[DataMatch] =
    filterResponses(user, "FILTER( xsd:decimal(?OBJ) >= 4 )")

  def filterBadQuestionnaires(user: User): Seq[DataMatch] =
    filterResponses(user, "FILTER( xsd:decimal(?OBJ) <= 3 )")

  /**
   * fonction qui compte les questionnaires dont toutes les réponses suivent un critère, tel que:
   * FILTER( xsd:decimal(?OBJ) >= 4 )
   *
   * @return liste des libellés des questionnaires
   * transactional
   */
  def filterResponses(user: User, sparqlCriterium: String): Seq[DataMatch] = {
    val resultTry = dataset.r({
      val userURI = getURI(user)
      val queryString = s"""
          ${declareSPARQL_PREFIX(xsd)}
          PREFIX rdfs: <${rdfs.prefixIri}>

          SELECT ?LAB ?dataURI
          WHERE {
           GRAPH <$userURI> {
             ?dataURI ?PROP ?OBJ .
             $sparqlCriterium
             ?dataURI a ?CLASS .
           }
           GRAPH ?ONTO {
             ?CLASS rdfs:label ?LAB.
           }
          } """
      import sparqlOps._
      import ops._
      val query = parseSelect(queryString).get
      val solutions = dataset.executeSelect(query, Map()).get
      val res = solutions.iterator map { row =>
        (row("LAB").get.as[Rdf#Literal].get,
          row("dataURI").get.as[Rdf#URI].get)
      }
      res.to[List].map { case (lab, uri) => (fromLiteral(lab)._1, uri) }
    })
    resultTry.get
  }

  def urlEncode(node: Any) = { URLEncoder.encode(node.toString, "utf-8") }
}