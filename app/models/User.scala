package models

import java.security.MessageDigest

import scala.collection.mutable.ArrayBuffer

import org.w3.banana.RDF
import org.w3.banana.RDFOps
import org.w3.banana.jena.Jena

import deductions.runtime.dataset.RDFStoreLocalProvider
import deductions.runtime.jena.ImplementationSettings
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import javax.xml.bind.annotation.adapters.HexBinaryAdapter


/**
 * data mapping between class User & RDF database;
 * this class has only services & has no data */
trait RDFUser[Rdf <: RDF, DATASET] extends RDFStoreLocalProvider[Rdf, DATASET]
with UserVocab[Rdf] {

  import ops._
  import rdfStore.transactorSyntax._
  import rdfStore.graphStoreSyntax._
  import rdfStore.sparqlEngineSyntax._

  def hashPassword(password: String): String = {
    new HexBinaryAdapter().marshal(MessageDigest.getInstance("MD5").digest(password.getBytes))
  }

  def checkPassword(user: User): Boolean = {
    User.find(user.email) match {
      case None => false
      case Some(registeredUser) =>
        registeredUser.passwordHash == hashPassword(user.password)
    }
  }

  /**
   * save User instance in RDF database;
   * email is a unique key, returns false if email already exists;
   *  transactional
   */
  def save(user: User): Boolean = {
    User.find(user.email) match {
      case Some(existingUser) => false
      case None =>
        val pgr = getURI(user) --
          bizinnovUserVocabPrefix("passwordHash") ->-
          makeLiteral(hashPassword(user.password), xsd.string) --
          bizinnovUserVocabPrefix("email") ->-
          makeLiteral(user.email, xsd.string)
        dataset.rw({
          dataset.appendToGraph(bizinnovUserGraphURI, pgr.graph)
        })
        UserData.createEmptyUserData(user)
        true
    }
  }

//  def makeURI(user: User) = bizinnovUserPrefix(user.email)

  /**
   * transactional
   *  TODO voir pour utiliser FormSaver
   */
  def saveInfo(user: User, info: UserCompanyInfo) = {
    dataset.rw({
      val userGraph = dataset.getGraph(bizinnovUserGraphURI).get
      val toDelete = ArrayBuffer[Rdf#Triple]()
      val triples = info.getMap.map({
        case (name, value) =>
          val existingValues = find(userGraph, getURI(user), bizinnovUserVocabPrefix(name), ANY)
          toDelete ++= existingValues
          makeTriple(
            getURI(user),
            bizinnovUserVocabPrefix(name),
            makeLiteral(value, xsd.string))
      })
      val graph = makeGraph(triples)
      dataset.removeTriples(bizinnovUserGraphURI, toDelete)
      dataset.appendToGraph(bizinnovUserGraphURI, graph)
    })
  }

  /** transactional */
  def getCompanyInfo(user: User): Option[UserCompanyInfo] = {
    val nonEntered = Some(UserCompanyInfo( Some("Non renseigné"), Some("9999"), Some("2222"),  Some("Non renseigné") ))
    dataset.r({
      val userGraph = dataset.getGraph(bizinnovUserGraphURI).get
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
        Some(UserCompanyInfo(department, naf, year, isGroup))
      } else {
        nonEntered
      }
    }).getOrElse(nonEntered)
  }
}

/** data class representing the users of the application */
case class User(val email: String, val password: String, val passwordHash: String = "")
    extends RDFStoreLocalJena1Provider
    with RDFUser[ Jena, ImplementationSettings.DATASET ] {
  
  def getURI() = bizinnovUserPrefix(email)
  def checkPassword(): Boolean = checkPassword(this)
}

/** gather URI's and prefixes for user management */
trait UserVocab[Rdf <: RDF] extends Prefixes[Rdf] {

  implicit val ops: RDFOps[Rdf]
  import ops._

  def getURI(user: User): Rdf#URI = bizinnovUserPrefix(user.email)

}

/** User lookup, using RDF store  */
object User extends RDFStoreLocalJena1Provider with UserVocab[Jena] {

  import ops._
  import rdfStore.transactorSyntax._
  import rdfStore.graphStoreSyntax._

  /** get user object from her Email;
   * NON transactional */
  def find(email: String): Option[User] = {
    val user = dataset.r({
      val userGraph = dataset.getGraph(bizinnovUserGraphURI).get
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
  
  /** get user object from from its URI ;
   * convenience method that does not populate user credentials */
  def getUserFromURI(userURI: String): User =
    User(bizinnovUserPrefix.unapply(URI(userURI)).get, "", "")
  
    def getUserURIFromEmail(email: String): String =
      fromUri( User( email, "", "").getURI() )
}
