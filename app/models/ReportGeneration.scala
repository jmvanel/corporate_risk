package models

import java.net.URLEncoder

import scala.collection.mutable.ArrayBuffer

import org.w3.banana.RDF

/**
 * (textual) Report Generation :
 *  see "Note on the data model" in README.md
 */

trait ReportGenerationTrait[Rdf <: RDF, DATASET]
    extends ResponseAnalysisTrait[Rdf, DATASET] {

  import ops._
  import rdfStore.transactorSyntax._
  import rdfStore.graphStoreSyntax._
  import rdfStore.sparqlEngineSyntax._

  /**
   * split Questionnaires in 2 sets: good, bad;
   *  transactional
   */
  def filterQuestionnaires(user: User, groupUri: String): (Seq[DataMatch] /*Good*/ , Seq[DataMatch] /*Bad*/ ) = {
    val good = ArrayBuffer[DataMatch]()
    val bad = ArrayBuffer[DataMatch]()
    val forms = getUserData(user, groupUri).map {
      case FormUserData(formUri, label) =>
        dataset.r({
          val uri = fromUri(formUri)
          val (note, _, label) = averagePerForm(user, uri)
          if (note >= 3.5)
            good.append((label, uri))
          else
            bad.append((label, uri))
        }).get
    }
    (good, bad)
  }

  //  /**
  //   * fonction qui compte les questionnaires dont toutes les réponses suivent un critère, tel que:
  //   * FILTER( xsd:decimal(?OBJ) >= 4 )
  //   *
  //   * @return liste des libellés des questionnaires
  //   * transactional
  //   */
  //  def filterResponses(user: User, sparqlCriterium: String): Seq[DataMatch] = {
  //    val resultTry = dataset.r({
  //      val userURI = getURI(user)
  //      val queryString = s"""
  //          ${declareSPARQL_PREFIX(xsd)}
  //          PREFIX rdfs: <${rdfs.prefixIri}>
  //
  //          SELECT ?LAB ?dataURI
  //          WHERE {
  //           GRAPH <$userURI> {
  //             ?dataURI ?PROP ?OBJ .
  //             $sparqlCriterium
  //             ?dataURI a ?CLASS .
  //           }
  //           GRAPH ?ONTO {
  //             ?CLASS rdfs:label ?LAB.
  //           }
  //          } """
  //      import sparqlOps._
  //      import ops._
  //      val query = parseSelect(queryString).get
  //      val solutions = dataset.executeSelect(query, Map()).get
  //      val res = solutions.iterator map { row =>
  //        (row("LAB").get.as[Rdf#Literal].get,
  //          row("dataURI").get.as[Rdf#URI].get)
  //      }
  //      res.to[List].map { case (lab, uri) => (fromLiteral(lab)._1, uri) }
  //    })
  //    resultTry.get
  //  }

  def urlEncode(node: Any) = { URLEncoder.encode(node.toString, "utf-8") }
}
