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
import org.w3.banana.SparqlOpsModule
import org.w3.banana.diesel._

/** Banana principle: refer to concrete implementation only in blocks without code */
object UserData extends RDFStoreLocalJena1Provider with UserDataTrait[Jena, Dataset]

trait UserDataTrait[Rdf <: RDF, DATASET] extends UserVocab
    with RDFStoreLocalProvider2[Rdf, DATASET]
    with InstanceLabelsInference2[Rdf]
    with SparqlGraphModule
    with SparqlOpsModule {

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
        for (classAndPropURI <- applicationClassesAndProperties().classesAndProperties)
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
          (cl, prop) <- applicationClassesAndProperties().classesAndProperties
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
  def applicationClassesAndProperties(formGroup: String = "risk"): //  Seq[(Rdf#URI, Rdf#URI)] 
  FormGroup = {
    formGroup match {
      case "risk" => FormGroup(applicationClassesAndPropertiesRisk,
        "Questions sur la gestion des risques.")
      //      case "nature" => applicationClassesAndPropertiesNature
      //      case "company" => applicationClassesAndPropertiesCompany
      //      case "brand" => applicationClassesAndPropertiesBrand
      case _ =>
        println(s"formGroup URI not expected: $formGroup");
        FormGroup(Seq((URI(""), URI(""))), "")
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
    val queryString = """
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      
      SELECT ?LAB ?PROP ?CLASS
      WHERE {
      a :FormGroup ; rdfs:label ?LAB ;
      :properties ?PROP .
      ?PROP rdfs:range ?CLASS .
      }
    """
    import sparqlOps._
    val query = parseSelect(queryString).get
    val solutions = rdfStore.executeSelect(dataset, query, Map()).get
    val variables: Iterator[(Rdf#Literal, Rdf#URI, Rdf#URI)] = solutions.iterator map { row =>
      /* row is an Rdf#Solution, we can get an Rdf#Node from the variable name
       * both the #Rdf#Node projection and the transformation to Rdf#URI can fail
       * in the Try type */
      (row("LAB").get.as[Rdf#Literal].get,
        row("CLASS").get.as[Rdf#URI].get,
        row("PROP").get.as[Rdf#URI].get)
    }
    println(variables.to[List].mkString("\n"))
    val classesAndProperties = for (v <- variables) yield (v._2, v._3)
    Seq(FormGroup(classesAndProperties.toSeq, fromLiteral(variables.next()._1)._1))
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