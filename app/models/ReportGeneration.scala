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

  def filterGoodQuestionnaires(user: User): Seq[String] =
    filterResponses(user, "FILTER( xsd:decimal(?OBJ) >= 4 )")

  def filterBadQuestionnaires(user: User): Seq[String] =
    filterResponses(user, "FILTER( xsd:decimal(?OBJ) <= 3 )")

  /**
   * fonction qui compte les questionnaires dont toutes les réponses suivent un critère, tel que:
   * FILTER( xsd:decimal(?OBJ) >= 4 )
   *
   * @return liste des questionnaires
   * transactional
   */
  def filterResponses(user: User, sparqlCriterium: String): Seq[String] = {
    val resultTry = dataset.r({
      val userURI = getURI(user)
      val queryString = s"""
          ${declareSPARQL_PREFIX(xsd)}
          SELECT ?LAB
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
        row("LAB").get.as[Rdf#Literal].get
      }
      res.toSeq.map { sol => fromLiteral(sol)._1 }
    })
    resultTry.get
  }
}