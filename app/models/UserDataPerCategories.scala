package models

import org.w3.banana.RDF
import deductions.runtime.services.SPARQLHelpers

trait UserDataPerCategories[Rdf <: RDF, DATASET]
    extends UserVocab[Rdf]
    with SPARQLHelpers[Rdf, DATASET] {

  import ops._

  /** transactional */
  def getUserDataInCategory(user: User, categoryUri: String): Seq[FormUserData[Rdf]] = {
    val userURI = getURI(user)

    val queryString = s"""
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX : <${bizinnovQuestionsVocabPrefix.prefixIri}>
      PREFIX dbo: <http://dbpedia.org/ontology/> 
 
      SELECT ?INSTANCE ?LAB
      # ques:capital-2 dbo:Type dbpedia:Organization .
      # ques:capital-2 a owl:Class ; rdfs:label "Capital Naturel"

      WHERE {
       GRAPH <$userURI> {
         ?INSTANCE a ?CLASS .
       }
       GRAPH ?ONTO {
         ?CLASS dbo:Type <$categoryUri> .
         ?CLASS rdfs:label ?LAB .
       }
      }
    """

    val res = sparqlSelectQueryVariables(
      queryString,
      Seq("INSTANCE", "LAB"))

    res.map {
      r =>
        FormUserData(
          foldNode(r(0))(
            u => u,
            _ => URI(""),
            _ => URI("")
          ), foldNode(r(1))(
            _ => "",
            _ => "",
            l => fromLiteral(l)._1
          ))
    }
  }
}
