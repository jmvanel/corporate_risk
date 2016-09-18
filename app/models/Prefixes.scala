package models

import org.w3.banana.RDF
import org.w3.banana.Prefix
import org.w3.banana.RDFOps

trait Prefixes[Rdf <: RDF] {

  implicit val ops: RDFOps[Rdf]
  import ops._

  /** prefix for URI resources of users */
  lazy val usersPrefix = "urn:/bizinnov/users/"
  
  /** users' id prefix */
  lazy val bizinnovUserPrefix = Prefix("usr", usersPrefix)
  
  /** URI of named graph for user information */
  lazy val bizinnovUserGraphURI = URI(usersPrefix) // URI(bizinnovUserPrefix.prefixIri)

  /** vocabulary for user properties: email, department, ... */
  lazy val bizinnovUserVocabPrefix = Prefix("user",
    "http://bizinnov.com/ontologies/users.owl.ttl#")

  /** Questionnaire vocabulary */
  lazy val bizinnovQuestionsVocabPrefix = Prefix("ques",
    "http://www.bizinnov.com/ontologies/quest.owl.ttl#")
  
   /** coherent with script populate_db.sh */
  val vocabularyGraph = "model:vocabulary"
  // TODO Add props & classes to Prefix objects
}