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

/** Stores data from the info form */
case class UserCompanyInfo(val department: Option[String] = None,
    val naf: Option[String] = None,
    val year: Option[String] = None,
    val isGroup: Option[String] = None) {

  def getMap =
    (Map[String, String]() /: this.getClass.getDeclaredFields) { (map, field) =>
      field.setAccessible(true)
      field.get(this).asInstanceOf[Option[String]] match {
        case None => map
        case Some(value) => {
          map + (field.getName -> value)
        }
      }
    }
}

/** TODO indiquer le but de la classe */
abstract class RDFUser[Rdf <: RDF](implicit ops: RDFOps[Rdf],
  rdfStore: RDFStore[Rdf, Try, RDFStoreObject.DATASET]) extends { // UserVocab {

  val rdfStoreObject = RDFStoreObject
  import ops._

  // TODO duplicate with UserVocab
  val bizinnovUserPrefix = Prefix("usr", "http://bizinnov.com/ontologies/users/")
  val bizinnovUserGraphURI = URI(bizinnovUserPrefix.prefixIri)
  val bizinnovUserVocabPrefix = Prefix("user", "http://bizinnov.com/ontologies/users.owl.ttl#")

  def hashPassword(password: String): String = {
    new HexBinaryAdapter().marshal(MessageDigest.getInstance("MD5").digest(password.getBytes))
  }

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
            makeURI(user),
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

  def saveInfo(user: User, info: UserCompanyInfo) = {
    val triples = info.getMap.map({
      case (name, value) =>
        makeTriple(
          makeURI(user),
          bizinnovUserVocabPrefix(name),
          makeLiteral(value, xsd.string))
    })
    val graph = makeGraph(triples)
    rdfStoreObject.rdfStore.rw(rdfStoreObject.dataset, {
      rdfStore.appendToGraph(rdfStoreObject.dataset, bizinnovUserGraphURI, graph)
    })
  }

  def getInfo(user: User): Option[UserCompanyInfo] = {
    rdfStoreObject.rdfStore.r(
      rdfStoreObject.dataset, {
        val userGraph = rdfStore.getGraph(rdfStoreObject.dataset, bizinnovUserGraphURI).get
        val userURI = getSubjects(userGraph,
          bizinnovUserVocabPrefix("email"),
          makeLiteral(user.email, xsd.string))
        if (!userURI.isEmpty) {
          val department = getObjects(userGraph, userURI.head,
            bizinnovUserVocabPrefix("department")).headOption.map(n => fromLiteral(n.asInstanceOf[Rdf#Literal])._1)
          val naf = getObjects(userGraph, userURI.head,
            bizinnovUserVocabPrefix("naf")).headOption.map(n => fromLiteral(n.asInstanceOf[Rdf#Literal])._1)
          val year = getObjects(userGraph, userURI.head,
            bizinnovUserVocabPrefix("year")).headOption.map(n => fromLiteral(n.asInstanceOf[Rdf#Literal])._1)
          val isGroup = getObjects(userGraph, userURI.head,
            bizinnovUserVocabPrefix("isGroup")).headOption.map(n => fromLiteral(n.asInstanceOf[Rdf#Literal])._1)
          Some(new UserCompanyInfo(department, naf, year, isGroup))
        } else {
          None
        }
      }).getOrElse(None)
  }
}

/** Class representing the users of the application */
case class User(val email: String, val password: String, val passwordHash: String = "")
    extends RDFUser[Jena] {
  def getURI() = bizinnovUserPrefix(email)
}

/** gather URI's and prefixes for user management */
trait UserVocab extends RDFOpsModule {
  import ops._
  /** users' prefix */
  lazy val bizinnovUserPrefix = Prefix("usr",
    "http://bizinnov.com/ontologies/users/")
  lazy val bizinnovUserGraphURI = URI(bizinnovUserPrefix.prefixIri)
  /** user vocabulary */
  lazy val bizinnovUserVocabPrefix = Prefix("user",
    "http://bizinnov.com/ontologies/users.owl.ttl#")
  /** Questionnaire vocabulary */
  lazy val bizinnovQuestionsVocabPrefix = Prefix("ques",
    "http://www.bizinnov.com/ontologies/quest.owl.ttl#")
  // TODO Add props & classes to Prefix objects
  def getURI(user: User) = bizinnovUserPrefix(user.email)

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