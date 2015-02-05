package models

import scala.xml.Elem

import org.w3.banana.RDF
import org.w3.banana.SparqlGraphModule
import org.w3.banana.SparqlOpsModule
import org.w3.banana.XSDPrefix
import org.w3.banana.diesel.toPointedGraphW
import org.w3.banana.jena.Jena

import com.hp.hpl.jena.query.Dataset

import deductions.runtime.abstract_syntax.InstanceLabelsInference2
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.jena.RDFStoreLocalProvider2

/** Responses Analysis */
object ResponseAnalysis extends RDFStoreLocalJena1Provider with ResponseAnalysisTrait[Jena, Dataset]

trait ResponseAnalysisTrait[Rdf <: RDF, DATASET] extends UserVocab
    with RDFStoreLocalProvider2[Rdf, DATASET]
    with InstanceLabelsInference2[Rdf]
    with SparqlGraphModule
    with SparqlOpsModule {

  val xsd = XSDPrefix[Rdf]
  /**
   * fonction qui compte les réponses pour une propriété de User,
   *  c'est à dire un formulaire, alias une rubrique (alias thème)
   */
  def responsesCount(user: User, propURI: String): Int = {
    val countTry = rdfStore.r(
      dataset, {
        val userURI = getURI(user)
        // NOTE: could have used find() like in UserData.getUserData()
        val queryString = s"""
      SELECT DISTINCT (COUNT(?OBJ) AS ?count) 
      WHERE {
       GRAPH <$userURI> {
         <$userURI> <$propURI> ?OBJ .
       }
      }
    """
        import sparqlOps._
        import ops._
        val query = parseSelect(queryString).get
        val solutions = rdfStore.executeSelect(dataset, query, Map()).get
        //     val variables: Iterator[(Rdf#Literal, Rdf#URI, Rdf#URI)] = solutions.iterator map { row =>
        val res = solutions.iterator map { row =>
          row("count").get.as[Rdf#Literal].get
        }
        res.next()
      })
    val lit = countTry.getOrElse(ops.makeLiteral("0", xsd.integer))
    ops.fromLiteral(lit)._1.toInt
  }

  /**
   * pour diagramme araignée, fonction qui chiffre chaque rubrique;
   *  renvoie aussi la somme des coefficients
   */
  def averagePerForm(propURI: String): (Int, Int) = (0, 1)

  /**fonction qui fournit un rapport en HTML */
  def report(): Elem = <p>Rapport de synthèse</p>
}