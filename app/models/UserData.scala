package models

import org.w3.banana.jena.JenaModule
import org.w3.banana.RDFOpsModule
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.dataset.RDFStoreLocalProvider
import org.w3.banana.RDF
import org.w3.banana.jena.Jena
import com.hp.hpl.jena.query.Dataset
import deductions.runtime.jena.RDFStoreObject
import deductions.runtime.abstract_syntax.UnfilledFormFactory
import deductions.runtime.abstract_syntax.InstanceLabelsInference2
import org.w3.banana.SparqlGraphModule
import org.w3.banana.SparqlOpsModule
import org.w3.banana.diesel._
import org.apache.log4j.Logger
import java.nio.file.StandardOpenOption

/** see function getUserData() */
case class FormUserData[Rdf <: RDF](data: Rdf#URI, label: String)

/** Banana principle: refer to concrete implementation only in blocks without code */
object UserData extends RDFStoreLocalJena1Provider with UserDataTrait[Jena, Dataset]

/** access to user data in triple store */
trait UserDataTrait[Rdf <: RDF, DATASET] extends UserVocab
    with RDFStoreLocalProvider[Rdf, DATASET]
    with InstanceLabelsInference2[Rdf]
    with SparqlGraphModule
    with SparqlOpsModule {

  import ops._

  /**
   * values for arguments to applicationClassesAndProperties(formGroup: String)
   *  TODO should be read from RDF database
   */
  lazy val formsGroups = List("risk", "capital") // human", "structural", "operational")
  lazy val formsGroupsURIs: List[Rdf#URI] = formsGroups map { fg => bizinnovQuestionsVocabPrefix(fg) }
  lazy val formsGroupsURIMap: Map[String, String] = formsGroups map { fg => fg -> fromUri(bizinnovQuestionsVocabPrefix(fg)) } toMap

  lazy val formGroupList: Map[String, String] = Map(
    "Pré-diagnostic" -> fromUri(bizinnovQuestionsVocabPrefix("risk")),
    "Diagnostic" -> fromUri(bizinnovQuestionsVocabPrefix("capital"))
  )
  /**
   * create Empty User Data : the triples:
   *  <pre>
   *  &lt; userURI> :prop-5 :v5 .
   *                    :v5 a ques:5 . # until ques:15
   *  </pre>
   *
   * create user data for all 4 Form Groups
   */
  def createEmptyUserData(user: User) = {
    rdfStore.rw(
      dataset, {
        for (fg <- formsGroups) {
          val cp = applicationClassesAndProperties(fg)
          println(s"createEmptyUserData $user $fg")
          for (classAndPropURI <- cp.classesAndProperties)
            createEmptyClassInstanceForUser(getURI(user), classAndPropURI)
        }
      })
  }

  /**
   * return User Data: a sequence of couples:
   * - an URI <u1> associated with the user <user> through one of the RDF properties <prop> in configuration :
   *     <user> <prop> <u1> .
   * - a label string associated to the class of <u1> in configuration.
   *
   * The configuration is gotten by function #applicationClassesAndProperties() .
   */
  def getUserData(user: User,
    formGroup: Rdf#URI = bizinnovQuestionsVocabPrefix("risk")): Seq[FormUserData[Rdf]] = {
    val nodes = rdfStore.r(dataset, {
      val userURI = getURI(user)
      val userGraph = rdfStore.getGraph(dataset, userURI).get
      implicit val graphForVocabulary = rdfStore.getGraph(dataset,
        URI("vocabulary")).get
      for {
        (cl, prop) <- applicationClassesAndProperties(formGroup).classesAndProperties
        triple <- find(userGraph, userURI, prop, ANY)
      } yield {
        (triple.objectt, instanceLabel(cl))
      }
    })
    val uriOptions = nodes.get.map {
      case (n, il) => foldNode(n)(
        uri => Some(uri, il),
        x => None, x => None)
    }
    val v = uriOptions collect { case Some((uri, il)) => (uri, il) }
    v.map(e => FormUserData(e._1, e._2))
  }

  /**
   * return a FormGroup, that is a sequence of URI couples:
   *  - an OWL class C,
   *  - and a property whose domain is :User and range C
   *
   *  Since each C is associated to a form, this defines the top-level structure of the user data input.
   */
  def applicationClassesAndProperties(formGroup: Rdf#URI): FormGroup = {
    applicationClassesAndProperties(questionsVocabURI2String(formGroup))
  }

  /** like before, different argument type */
  def applicationClassesAndProperties(formGroupName: String): FormGroup = {
    formGroupName match {
      case "risk" => FormGroup(applicationClassesAndPropertiesRisk,
        "Questions sur la gestion des risques.")
      case name => classesAndProperties(name + "-fg")
      //      case _ =>
      //        println(s"formGroup URI not expected: $formGroup");
      //        FormGroup(Seq((URI(""), URI(""))), "")
    }
  }

  //  def getPropertiesInFormGroup(formGroup: String): Seq[String] = {
  //    val nodes = rdfStore.r(dataset, {
  //      val graphForVocabulary = rdfStore.getGraph(dataset, URI("vocabulary")).get
  //      val triples = find(graphForVocabulary, bizinnovQuestionsVocabPrefix(formGroup), bizinnovQuestionsVocabPrefix("properties"), ANY)
  //      triples.map { triple => triple.toString() }.toSeq
  //    })
  //    nodes.get
  //  }

  private def questionsVocabURI2String(uri: Rdf#URI): String = bizinnovQuestionsVocabPrefix.unapply(uri).getOrElse("")

  private def applicationClassesAndPropertiesRisk(): Seq[(Rdf#URI, Rdf#URI)] = {
    val range = 5 until 16
    for (i <- range) yield (bizinnovQuestionsVocabPrefix(i.toString()),
      bizinnovQuestionsVocabPrefix("prop-" + i.toString()))
  }

  /** @param fg is URI ending for forms group */
  private def classesAndProperties(fg: String): FormGroup = {
    applicationClassesAndPropertiesGeneric(
      fromUri(bizinnovQuestionsVocabPrefix(fg)))
  }

  /** see #applicationClassesAndProperties() */
  case class FormGroup(val classesAndProperties: Seq[(Rdf#URI, Rdf#URI)], label: String)

  /**
   * Detect RDF patterns like:
   * <pre>
   * &lt;formgroup> a :FormGroup ;
   * rdfs:label "Questions sur la gestion des risques."@fr ;
   * :properties :p1, :p2 .
   * </pre>
   * and return a list of couples (:p1, rdfs:range of :p1) .
   */
  private def applicationClassesAndPropertiesGeneric(formgroup: String): FormGroup = {
    val queryString = s"""
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX : <${bizinnovQuestionsVocabPrefix.prefixIri}>
      SELECT ?LAB ?PROP ?CLASS
      WHERE {
#      GRAPH <vocabulary> {
       GRAPH ?G {
        <$formgroup> a :FormGroup ; rdfs:label ?LAB ;
        :properties ?PROP .
        ?PROP rdfs:range ?CLASS .
       }
      }
    """
    import sparqlOps._
    val query = parseSelect(queryString).get
    val solutions = rdfStore.executeSelect(dataset, query, Map()).get
    var label = Literal("")
    val variables: Iterator[(Rdf#Literal, Rdf#URI, Rdf#URI)] = solutions.iterator map { row =>
      /* row is an Rdf#Solution, we can get an Rdf#Node from the variable name
       * both the #Rdf#Node projection and the transformation to Rdf#URI can fail
       * in the Try type */
      label = row("LAB").get.as[Rdf#Literal].get
      (
        label, // TODO remove label in tuple
        row("CLASS").get.as[Rdf#URI].get,
        row("PROP").get.as[Rdf#URI].get)
    }
    println("applicationClassesAndPropertiesGeneric")
    val classesAndProperties = for (v <- variables) yield (v._2, v._3)
    val fg = FormGroup(classesAndProperties.to[List], fromLiteral(label)._1
    )
    //    println("classesAndProperties " + classesAndProperties.mkString("\n"))
    println(s"$formgroup $fg")
    fg
  }

  def println(mess: String) = {
    Logger.getRootLogger().info(mess)
    val fileName = "bblog.txt"
    import java.nio.file.{ Paths, Files }
    import java.nio.charset.StandardCharsets
    Files.write(Paths.get(fileName), (mess + "\n").getBytes(
      StandardCharsets.UTF_8), StandardOpenOption.APPEND, StandardOpenOption.CREATE
    )
  }

  private def createEmptyClassInstanceForUser(userURI: Rdf#URI, classAndPropURI: (Rdf#URI, Rdf#URI)) = {
    val newURI = URI(UnfilledFormFactory.makeId(userURI.toString()))
    val graph = makeGraph(List(
      makeTriple(userURI, classAndPropURI._2, newURI)))
    rdfStore.appendToGraph(dataset, userURI, graph)
    println(s"createEmptyClassInstanceForUser $userURI $graph")
    println(s"createEmptyClassInstanceForUser $classAndPropURI")
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