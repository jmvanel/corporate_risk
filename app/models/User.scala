package models

import play.api._
import org.w3.banana._
import org.w3.banana.jena.JenaModule
import java.security.MessageDigest
import deductions.runtime.jena.RDFStoreObject
import scala.util.Try
import org.w3.banana.jena.Jena

/** TODO indiquer le but de la classe */
// trait RDFUser extends RDFModule with RDFOpsModule {
abstract class RDFUser[Rdf <: RDF](implicit ops: RDFOps[Rdf],
                                                    rdfStore: RDFStore[Rdf, Try,
                                                      RDFStoreObject.DATASET
                                                      ]) {
  val rdfStoreObject = RDFStoreObject
  import ops._
  import RDFStoreUser._
  val bizinnovUserGraphURI = URI("http://bizinnov.com/ontologies/users/")

  def checkPassword(password: String): Boolean = {
    val passwordDigest = MessageDigest.getInstance("MD5").digest(password.getBytes)
    val passwordFromRDFStore = "" // TODO <<<<<<<<<<<<<<<
    passwordFromRDFStore == passwordDigest
  }

  def save(user: User) = {
    val triples = List(
      makeTriple(
        makeUri(bizinnovUserVocabPrefix + user.email),
        makeUri(bizinnovUserVocabPrefix + "email"),
        makeUri(bizinnovUserVocabPrefix + user.email)),
      makeTriple(
    		  makeUri(bizinnovUserVocabPrefix + user.email),
        makeUri(bizinnovUserVocabPrefix + "password"),
        makeUri(bizinnovUserVocabPrefix + MessageDigest.getInstance("MD5").digest(user.password.getBytes))))
    val graph = makeGraph(triples)
    rdfStoreObject.rdfStore.rw(rdfStoreObject.dataset, {
      rdfStore.appendToGraph(rdfStoreObject.dataset, bizinnovUserGraphURI, graph)
    })
  }
}

/** TODO indiquer le but de la classe */
class User(var email: String, var password: String)
extends RDFUser[Jena]

/** User lookup, using RDF store  */
object User extends JenaModule {
  import ops._
  val bizinnovUserVocabPrefix = "http://bizinnov.com/ontologies/users.owl.ttl#"
  val bizinnovUserGraphURI = URI("http://bizinnov.com/ontologies/users/")
    val rdfStoreObject = RDFStoreObject
    def find(email: String): Option[User] = {
    	rdfStoreObject.rdfStore.r(rdfStoreObject.dataset, {
      val userGraph = rdfStore.getGraph( rdfStoreObject.dataset, bizinnovUserGraphURI).get
      // TODO null pas beau !!!!!!!!!!!!!!
      val triples = ops.getObjects( userGraph, URI(bizinnovUserVocabPrefix+email), null )
    })
//    if( triples.size > 0 )
//      Some(???)
//     else
      None // TODO <<<<<<<<<
    }
}