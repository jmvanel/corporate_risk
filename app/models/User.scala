package models

import scala.util.Try

import org.w3.banana.Prefix
import org.w3.banana.RDF
import org.w3.banana.RDFOps
import org.w3.banana.RDFOpsModule
import org.w3.banana.RDFStore
import org.w3.banana.jena.Jena
import org.w3.banana.jena.JenaModule

import deductions.runtime.jena.RDFStoreObject

import java.security.MessageDigest
import javax.xml.bind.annotation.adapters.HexBinaryAdapter

/** TODO indiquer le but de la classe */
abstract class RDFUser[Rdf <: RDF](implicit ops: RDFOps[Rdf],
    rdfStore: RDFStore[Rdf, Try, RDFStoreObject.DATASET]) // extends UserVocab
    {

  val rdfStoreObject = RDFStoreObject
  import ops._

  def hashPassword(password: String): String = {
    new HexBinaryAdapter().marshal(MessageDigest.getInstance("MD5").digest(password.getBytes))
  }

  // TODO duplicate with UserVocab
  val bizinnovUserPrefix = Prefix("usr", "http://bizinnov.com/ontologies/users/")
  val bizinnovUserGraphURI = URI(bizinnovUserPrefix.prefixIri)
  val bizinnovUserVocabPrefix = Prefix("user", "http://bizinnov.com/ontologies/users.owl.ttl#")

  def checkPassword(user: User): Boolean = {
    User.find(user.email) match {
      case None => false
      case Some(registeredUser) => registeredUser.passwordHash ==
        hashPassword(user.password)
    }
  }

  def save(user: User): Boolean = {
    User.find(user.email) match {
      case Some(existingUser) => false
      case None =>
        val triples = List(
          makeTriple(
            bizinnovUserPrefix(user.email),
            bizinnovUserVocabPrefix("passwordHash"),
            makeLiteral(hashPassword(user.password), xsd.string)),
          makeTriple(
            makeURI(user),
            bizinnovUserVocabPrefix("email"),
            makeLiteral(user.email, xsd.string)
          ))
        val graph = makeGraph(triples)
        rdfStoreObject.rdfStore.rw(rdfStoreObject.dataset, {
          rdfStore.appendToGraph(rdfStoreObject.dataset, bizinnovUserGraphURI, graph)
        })
        UserData.createEmptyUserData(user)
        true
    }
  }

  def makeURI(user: User) = bizinnovUserPrefix(user.email)
}

/** Class representing the users of the application */
case class User(var email: String, var password: String, var passwordHash: String = "")
    extends RDFUser[Jena] {
  //	def makeURI() = bizinnovUserPrefix(email)
}

/** gather URI's and prefixes for user management */
trait UserVocab extends RDFOpsModule {
  import ops._
  /** users' prefix */
  val bizinnovUserPrefix = Prefix("usr",
    "http://bizinnov.com/ontologies/users/")
  val bizinnovUserGraphURI = URI(bizinnovUserPrefix.prefixIri)
  /** user vocabulary */
  val bizinnovUserVocabPrefix = Prefix("user",
    "http://bizinnov.com/ontologies/users.owl.ttl#")
  /** Questionnaire vocabulary */
  val bizinnovQuestionsVocabPrefix = Prefix("ques",
    "http://www.bizinnov.com/ontologies/quest.owl.ttl#")
  // TODO Add props & classes to Prefix objects
  def makeURI(user: User) = bizinnovUserPrefix(user.email)

}

/** User lookup, using RDF store  */
object User extends JenaModule with UserVocab {
  import ops._
  val rdfStoreObject = RDFStoreObject

  def find(email: String): Option[User] = {
    val user = rdfStoreObject.rdfStore.r(
      rdfStoreObject.dataset, {
        val userGraph = rdfStore.getGraph(rdfStoreObject.dataset, bizinnovUserGraphURI).get
        val userURI = getSubjects(userGraph,
          bizinnovUserVocabPrefix("email"),
          makeLiteral(email, xsd.string))
        if (!userURI.isEmpty) {
          val passwordHash = getObjects(userGraph, userURI.head,
            bizinnovUserVocabPrefix("passwordHash"))
          val email = getObjects(userGraph, userURI.head,
            bizinnovUserVocabPrefix("email"))
          if (!passwordHash.isEmpty) {
            val userEmail = foldNode(email.head)(_ => "", _ => "", l => fromLiteral(l)_1)
            val userPasswordHash = foldNode(passwordHash.head)(_ => "", _ => "", l => fromLiteral(l)._1)
            Some(User(email = userEmail, password = "", passwordHash = userPasswordHash))
          } else None
        } else None
      })
    user.getOrElse(None)
  }
}