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
import deductions.runtime.abstract_syntax.UnfilledFormFactory

/** Banana principle: refer to concrete implementation only in blocks without code */
object UserData extends RDFStoreLocalJena1Provider with UserDataTrait[Jena, Dataset]

trait UserDataTrait[Rdf <: RDF, DATASET] extends UserVocab
    with RDFStoreLocalProvider2[Rdf, DATASET] {

  import ops._

  /**
   * create Empty User Data : triples:
   *  <pre>
   *  <userURI> :prop-5 :v5 .
   *                    :v5 a ques:5 . # until ques:15
   *  </pre>
   *
   * :prop-5     a owl:ObjectProperty; rdfs:domain :User; rdfs:range :5 .
   *
   */
  def createEmptyUserData(user: User) = {
    // enumerate classes in vocabulary/risk/risk_questions.owl.ttl
    // TODO read RDF configuration for this, like is done for classes themselves 
    rdfStore.rw(
      dataset, {
        for (classAndPropURI <- applicationClasses())
          createEmptyClassInstances(bizinnovUserPrefix(user.email), classAndPropURI)
      })
  }

  def applicationClasses() = {
    val range = 5 until 16
    for (i <- range) yield (bizinnovQuestionsVocabPrefix(i.toString()),
      bizinnovQuestionsVocabPrefix("prop-" + i.toString()))
  }

  private def createEmptyClassInstances(userURI: Rdf#URI, classAndPropURI: (Rdf#URI, Rdf#URI)) = {
    val newURI = URI(UnfilledFormFactory.makeId(userURI.toString()))
    val graph = makeGraph(List(
      makeTriple(userURI, classAndPropURI._2, newURI)))
    rdfStore.appendToGraph(dataset, userURI, graph)
    createEmptyClassInstance(newURI, classAndPropURI._1, userURI)
  }

  private def createEmptyClassInstance(subjectURI: Rdf#URI, classURI: Rdf#URI,
    graphURI: Rdf#URI) = {
    println(s"create Empty Class $classURI Instance for subject URI $subjectURI")
    val graph = makeGraph(List(
      makeTriple(subjectURI, rdf.typ, classURI)))
    // TODO call appendToGraph only once
    rdfStore.appendToGraph(dataset, graphURI, graph)
  }
}