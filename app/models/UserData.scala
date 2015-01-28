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
import deductions.runtime.abstract_syntax.InstanceLabelsInference2
import org.w3.banana.SparqlGraphModule

/** Banana principle: refer to concrete implementation only in blocks without code */
object UserData extends RDFStoreLocalJena1Provider with UserDataTrait[Jena, Dataset]

trait UserDataTrait[Rdf <: RDF, DATASET] extends UserVocab
    with RDFStoreLocalProvider2[Rdf, DATASET]
    with InstanceLabelsInference2[Rdf]
    with SparqlGraphModule {

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
    // enumerate classes in graph "vocabulary"
    // TODO read RDF configuration for this, like is done for classes themselves 
    rdfStore.rw(
      dataset, {
        for (classAndPropURI <- applicationClassesAndProperties())
          createEmptyClassInstanceForUser(getURI(user), classAndPropURI)
      })
  }

  /**
   * return a sequence of couples:
   * - an URI <u1> associated with the user <user> through one of the RDF properties <prop> in configuration :
   *     <user> <prop> <u1> .
   * - a label string associated to the class of <u1> in configuration.
   *
   * The configuration is gotten by function #applicationClassesAndProperties() .
   */
  def getUserData(user: User): Seq[(Rdf#URI, String)] = {
    val nodes = rdfStore.r(
      dataset, {
        val userURI = getURI(user)
        val graph = rdfStore.getGraph(dataset, userURI).get
        implicit val graphForVocabulary = rdfStore.getGraph(dataset,
          URI("vocabulary")).get
        for {
          (cl, prop) <- applicationClassesAndProperties()
          triple <- find(graph, userURI, prop, ANY)
        } yield {
          (triple.objectt, instanceLabel(cl))
        }
      })
    val uriOptions = nodes.get.map {
      case (n, il) => foldNode(n)(
        uri => Some(uri, il),
        x => None, x => None)
    }
    uriOptions collect { case Some((uri, il)) => (uri, il) }
  }

  /**
   * return a sequence of URI couples:
   *  - an OWL class C,
   *  - and a property whose domain is :User and range C
   *
   *  Since each C is associated to a form, this defines the top-level structure of the user data input.
   */
  def applicationClassesAndProperties(formGroup: String = "risk"): Seq[(Rdf#URI, Rdf#URI)] = {
    formGroup match {
      case "risk" => applicationClassesAndPropertiesRisk
      case "nature" => applicationClassesAndPropertiesNature
      case "company" => applicationClassesAndPropertiesCompany
      case "brand" => applicationClassesAndPropertiesBrand
      case _ => println(s"formGroup URI not expected: $formGroup"); Seq((URI(""), URI("")))
    }
  }

  def applicationClassesAndPropertiesRisk(): Seq[(Rdf#URI, Rdf#URI)] = {
    val range = 5 until 16
    for (i <- range) yield (bizinnovQuestionsVocabPrefix(i.toString()),
      bizinnovQuestionsVocabPrefix("prop-" + i.toString()))
  }

  def applicationClassesAndPropertiesNature(): Seq[(Rdf#URI, Rdf#URI)] = {
    applicationClassesAndPropertiesGeneric
    Seq((URI(""), URI(""))) // TODO <<<<<<<<<<<
  }

  def applicationClassesAndPropertiesCompany(): Seq[(Rdf#URI, Rdf#URI)] = {
    applicationClassesAndPropertiesGeneric
    Seq((URI(""), URI(""))) // TODO <<<<<<<<<<<
  }

  def applicationClassesAndPropertiesBrand(): Seq[(Rdf#URI, Rdf#URI)] = {
    Seq((URI(""), URI(""))) // TODO <<<<<<<<<<<
  }

  case class FormGroup(val classesAndProperties: Seq[(Rdf#URI, Rdf#URI)], label: String)

  /**
   * Detect RDF patterns like:
   * <pre>
   * :risk-fg a :FormGroup ;
   * rdfs:label "Questions sur la gestion des risques."@fr ;
   * :properties :p1, :p2 .
   * </pre>
   * and return a list of couples (:p1, rdfs:range of :p1) .
   */
  def applicationClassesAndPropertiesGeneric(): Seq[FormGroup] = {
    Seq(FormGroup(Seq((URI(""), URI(""))), "")) // TODO <<<<<<<<<<<
    /*  
  def executeSelect(a: A, query: Rdf#SelectQuery, bindings: Map[String, Rdf#Node]): M[Rdf#Solutions]
  def executeConstruct(a: A, query: Rdf#ConstructQuery, bindings: Map[String, Rdf#Node]): M[Rdf#Graph]
       */
  }

  private def createEmptyClassInstanceForUser(userURI: Rdf#URI, classAndPropURI: (Rdf#URI, Rdf#URI)) = {
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