package models

import org.w3.banana.RDF
import org.w3.banana.Prefix
import org.w3.banana.RDFOps

trait Prefixes[Rdf <: RDF] {

  implicit val ops: RDFOps[Rdf]
  import ops._

  /** users' prefix */
  lazy val bizinnovUserPrefix = Prefix("usr", User.usersPrefix)
  lazy val bizinnovUserGraphURI = URI(bizinnovUserPrefix.prefixIri)
  /** user vocabulary */
  lazy val bizinnovUserVocabPrefix = Prefix("user",
    "http://bizinnov.com/ontologies/users.owl.ttl#")
  /** Questionnaire vocabulary */
  lazy val bizinnovQuestionsVocabPrefix = Prefix("ques",
    "http://www.bizinnov.com/ontologies/quest.owl.ttl#")
    
  // TODO Add props & classes to Prefix objects
  
  /** prefix for URI resources of users */
  val usersPrefix = "urn:/bizinnov/users/"
}