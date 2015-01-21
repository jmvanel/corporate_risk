package models

import org.w3.banana.jena.JenaModule
import org.w3.banana.RDFOpsModule
import deductions.runtime.jena.RDFStoreLocalProvider
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.jena.RDFStoreLocalProvider2
import org.w3.banana.RDF
import org.w3.banana.jena.Jena
import com.hp.hpl.jena.query.Dataset
import deductions.runtime.jena.RDFStoreObject

/** Banana principle: refer to concrete implementation only in  */
object UserData extends RDFStoreLocalJena1Provider with UserDataTrait[Jena, Dataset]

trait UserDataTrait[Rdf <: RDF, DATASET] extends UserVocab
  with RDFStoreLocalProvider2[Rdf, DATASET] {

  import ops._

  /** create Empty User Data :
   *  just triples:
   *  <userURI> a ques:5 , ques:6 . # until ques:15 */
  def createEmptyUserData(user: User) = {
    // enumerate classes in vocabulary/risk/risk_questions.owl.ttl
    // TODO read RDF configuration for this, like is done for classes themselves 
    rdfStore.rw(
      dataset, {
        val range = 5 until 16
        for (i <- range)
          createEmptyClassInstance(bizinnovUserPrefix(user.email),
            bizinnovQuestionsVocabPrefix(i.toString()))
      })
  }

  private def createEmptyClassInstance(userURI: Rdf#URI, classURI: Rdf#URI) = {
    println(s"create Empty Class $classURI Instance for user $userURI")
    val graph = makeGraph( List(
    makeTriple(
      userURI,
      rdf.typ,
      classURI)
      ))
    val userGraphURI = userURI
    // TODO call appendToGraph only once
    rdfStore.appendToGraph( dataset, userGraphURI, graph)
  }
}