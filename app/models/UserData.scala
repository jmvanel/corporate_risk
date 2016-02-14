package models

import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption

import org.apache.log4j.Logger
import org.w3.banana.PointedGraph
import org.w3.banana.RDF
import org.w3.banana.RDFSPrefix
import org.w3.banana.jena.Jena

import com.hp.hpl.jena.query.Dataset

import deductions.runtime.abstract_syntax.InstanceLabelsInferenceMemory
import deductions.runtime.abstract_syntax.PreferredLanguageLiteral
import deductions.runtime.abstract_syntax.UnfilledFormFactory
import deductions.runtime.dataset.RDFStoreLocalProvider
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.sparql_cache.RDFCacheAlgo

/** an URI and a label, see function getUserData() */
case class FormUserData[Rdf <: RDF](data: Rdf#URI, label: String)

/** Banana principle: refer to concrete implementation only in blocks without code */
object UserData extends RDFStoreLocalJena1Provider
with UserDataTrait[Jena, Dataset]
with ResponseAnalysisTrait[Jena, Dataset]

/** access to user data in triple store */
trait UserDataTrait[Rdf <: RDF, DATASET] extends UserVocab[Rdf]
    with RDFStoreLocalProvider[Rdf, DATASET]
    with RDFCacheAlgo[Rdf, DATASET]
    with PreferredLanguageLiteral[Rdf]
    with InstanceLabelsInferenceMemory[Rdf, DATASET]
    with FormsGroupsData1[Rdf] {

  /** coherent with script populate_db.sh */
  val vocabularyGraph = "model:vocabulary"

  import ops._
  import rdfStore.transactorSyntax._
  import rdfStore.graphStoreSyntax._
  import rdfStore.sparqlEngineSyntax._

  /**
   * create empty user managed Data for all 4 Form Groups : the triples:
   *  <pre>
   *  &lt; userURI> :prop-5 :v5 .
   *                    :v5 a ques:5 . # until ques:15
   *  </pre>
   *
   * transactional
   */
  def createEmptyUserData(user: User) = {
    dataset.rw({
      for (fg <- formsGroups) {
        val cp = applicationClassesAndProperties(fg)
        println(s"createEmptyUserData $user $fg $cp")
        for (classAndPropURI <- cp.classesAndProperties)
          createEmptyClassInstanceForUser(getURI(user), classAndPropURI)
      }
    })
  }

  /**
   * return all (direct) User Data: a sequence of couples:
   * - an URI <u1> associated with the user <user> through one of the RDF properties <prop> in configuration :
   *     <user> <prop> <u1> .
   * - a label string associated to the class of <u1> in configuration.
   *
   * The configuration (data model & form groups) is gotten by function #applicationClassesAndProperties() .
   *
   * transactional
   */
  def getUserData(user: User, formGroupUri: String = ""): Seq[FormUserData[Rdf]] = {
    println(s"getUserData: formGroupUri $formGroupUri")
    val formGroup = formGroupUri match {
      case "" => URI(formsGroupsURIMap("risk"))
      // bizinnovQuestionsVocabPrefix("risk")
      case uri: String => URI(uri)
    }
    // rw because of instanceLabel()
    val nodesAndLabels = dataset.rw({
      val userURI = getURI(user)
      val userGraph = dataset.getGraph(userURI).get
      val classesAndProperties =
        applicationClassesAndProperties(formGroup).classesAndProperties
      println(s"getUserData classesAndProperties $classesAndProperties")
      val objectAndClass = for {
        (classe, prop) <- classesAndProperties
        //        debug = println(s"getUserData ($cl, $prop)")
        triple <- find(userGraph, userURI, prop, ANY)
        //        debug2 = println(s"getUserData $triple")
      } yield (triple.objectt, classe)
      //      println(s"objectAndClass ${objectAndClass.mkString(", ")}")

      implicit val graphForVocabulary = dataset.getGraph(URI(vocabularyGraph)).get
      val nodesAndLabels = for {
        (objectt, classe) <- objectAndClass
      } yield (objectt, instanceLabel(classe, graphForVocabulary, "" /*lang TODO*/ ))
      nodesAndLabels . toList
    })

    //    println(s"nodesAndLabels ${nodesAndLabels.get.mkString(", ")}")
    val uriOptions = nodesAndLabels.get.map {
      case (n, il) => foldNode(n)(
        uri => Some(uri, il),
        x => None, x => None)
    }
    //    println(s"uriOptions ${uriOptions.mkString(", ")}")
    val valuesAndLabels = uriOptions collect { case Some((uri, il)) => (uri, il) }
    println(s"valuesAndLabels ${valuesAndLabels.mkString(", ")}")
    valuesAndLabels.map(e => FormUserData(e._1, e._2))
  }

  /**
   * get User Data for all form Groups
   * transactional
   */
  def getAllUserData(user: User): Seq[FormUserData[Rdf]] =
    for (
      fg <- formsGroupsURIMap.values.toSeq;
      fud <- getUserData(user, fg.toString)
    ) yield fud

  /** transactional */
  def getPreviousForm(user: User, dataURI: String): Option[FormUserData[Rdf]] = {
    val form = for (
      Seq(f1, f2) <- getAllUserData(user).sliding(2);
      if (fromUri(f2.data) == dataURI)
    ) yield f1

    if (form isEmpty) None else Some(form.next)
  }
  /** transactional */
  def getNextForm(user: User, dataURI: String): Option[FormUserData[Rdf]] = {
    val fuds = getAllUserData(user)
    infor(s"""getNextForm for dataURI : ${dataURI} """)
    infor(s"""getNextForm total for all groups : ${fuds.size} """)
    val sl = fuds.sliding(2)

    val nf = for (
      Seq(f1, f2) <- sl;
      if (fromUri(f1.data) == dataURI)
    ) yield f2
    infor(s"""getNextForm $dataURI : $nf """)
    if (nf isEmpty) None else Some(nf.next)
  }

  def getFormLabel(formUri: String): String = {
    dataset.r({
      val graph = allNamedGraph
      // possible user-specific label:
      val userSpecificLabel = instanceLabel(URI(formUri), graph, "" /*lang TODO*/ )
      if( formUri.endsWith( userSpecificLabel ) ) {
        // get label from class
        import org.w3.banana.PointedGraph
        import org.w3.banana.syntax._
        val pg = PointedGraph(URI(formUri), allNamedGraph)
        val rdfs = RDFSPrefix[Rdf]
        val lab = pg / rdf.typ / rdfs.label
        lab.nodes.headOption match {
          case Some(n) => n.toString()
          case None => "? " + formUri
        }
      } else userSpecificLabel
    }).get
  }

  /**
   * get Form Group name corresponding to User & data URI;
   *  see #getUserData ; transactional
   */
  def getFormGroup(user: User, dataURI: String): String = {
    val formsGroupsURIs = formsGroupsURIMap.values.toSeq
    println(s"getFormGroup ${formsGroupsURIs.mkString(", ")}")
    val userDataGroups = for (fg <- formsGroupsURIs) yield getUserData(user, fg)
    println(s"getFormGroup ${userDataGroups.mkString(", ")}")
    val userDataGroup = userDataGroups.find {
      udg =>
        val userData = udg.find {
          formUserData => formUserData.data.toString() == dataURI
        }
        println(s"getFormGroup userData.isDefined ${userData.isDefined} udg ${udg.mkString(", ")}")
        userData.isDefined
    }
    if (userDataGroup.isDefined) {
      val x = userDataGroup.get
      val ind = userDataGroups.indexOf(x)
      formsGroupsURIs(ind)
    } else ""
  }

  //  private
  def infor(s: String) = Logger.getRootLogger().info(s)

  /** @argument formGroup URI like eg http://www.bizinnov.com/ontologies/quest.owl.ttl#capital-fg
   * @return a FormGroup, that is a sequence of URI couples:
   *  - an OWL class C,
   *  - and a property whose domain is :User and range C
   *
   *  Since each C is associated to a form, this defines the top-level structure of the user data input.
   *  See "Note on the data model" in README.md
   */
  def applicationClassesAndProperties(formGroup: Rdf#URI): FormGroup = {
    infor(s"""applicationClassesAndProperties formGroupName Rdf#URI <$formGroup> """)
    infor(s"""applicationClassesAndProperties bizinnovQuestionsVocabPrefix $bizinnovQuestionsVocabPrefix """)
    applicationClassesAndProperties(questionsVocabURI2String(formGroup))
  }

  /** like before, different argument type (eg "risk-fg")
   * NON transactional */
  def applicationClassesAndProperties(formGroupName: String): FormGroup = {
    infor(s"""applicationClassesAndProperties formGroupName String "$formGroupName" """)
    formGroupName match {
      case name if (name.startsWith("risk")) =>
        FormGroup(applicationClassesAndPropertiesRisk,
            "Questions sur la gestion des risques.")
      case name => classesAndProperties(name)
    }
  }

  private def questionsVocabURI2String(uri: Rdf#URI): String = bizinnovQuestionsVocabPrefix.unapply(uri).getOrElse("")

  private def applicationClassesAndPropertiesRisk(): Seq[(Rdf#URI, Rdf#URI)] = {
    val range = 5 until 16
    for (i <- range) yield (bizinnovQuestionsVocabPrefix(i.toString()),
      bizinnovQuestionsVocabPrefix("prop-" + i.toString()))
  }

  /** @param fg is URI ending for forms group (eg "risk-fg")
   * NON transactional */
  private def classesAndProperties(fg: String): FormGroup = {
    applicationClassesAndPropertiesGeneric(
      if (formsGroupsURIMap.contains(fg))
        formsGroupsURIMap(fg)
      else
        fromUri(bizinnovQuestionsVocabPrefix(fg))
    )
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
   *
   * NON transactional
   */
  private def applicationClassesAndPropertiesGeneric(formgroup: String): FormGroup = {
    val queryString = s"""
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX : <${bizinnovQuestionsVocabPrefix.prefixIri}>
      SELECT ?LAB ?PROP ?CLASS
      WHERE { # GRAPH <$vocabularyGraph> {
       GRAPH ?G {
        <$formgroup> a :FormGroup ; rdfs:label ?LAB ;
        :properties ?PROP .
        ?PROP rdfs:range ?CLASS .
       }
      }
    """
    import sparqlOps._
    val query = parseSelect(queryString).get
    val solutions = dataset.executeSelect(query, Map()).get
    var label = Literal("")
    val variables0: Iterator[(Rdf#Literal, Rdf#URI, Rdf#URI)] = solutions.iterator map {
          row =>
      /* row is an Rdf#Solution, we can get an Rdf#Node from the variable name
       * both the #Rdf#Node projection and the transformation to Rdf#URI can fail
       * in the Try type */
      label = row("LAB").get.as[Rdf#Literal].get
      (
        label, // TODO remove label in tuple
        row("CLASS").get.as[Rdf#URI].get,
        row("PROP").get.as[Rdf#URI].get)
    }
    val variables = variables0 . toList
    println("applicationClassesAndPropertiesGeneric")
    val classesAndProperties = for (v <- variables) yield (v._2, v._3)
    val fg = FormGroup(classesAndProperties.to[List], fromLiteral(label)._1
    )
    //    println("classesAndProperties " + classesAndProperties.mkString("\n"))
    println(s"\t formgroup = $formgroup : $fg")
    fg
  }

  def println(mess: String) = {
    infor(mess)
    val fileName = "bblog.txt"
    import java.nio.file.{ Paths, Files }
    import java.nio.charset.StandardCharsets
    Files.write(Paths.get(fileName), (mess + "\n").getBytes(
      StandardCharsets.UTF_8), StandardOpenOption.APPEND, StandardOpenOption.CREATE
    )
  }

  /** NON transactional */
  private def createEmptyClassInstanceForUser(userURI: Rdf#URI,
      classAndPropURI: (Rdf#URI, Rdf#URI)) = {
    val newURI = URI(UnfilledFormFactory.makeId(userURI.toString()))
    val graph = makeGraph(List(
      makeTriple(userURI, classAndPropURI._2, newURI)))
    dataset.appendToGraph(userURI, graph)
    println(s"createEmptyClassInstanceForUser $userURI $graph")
    println(s"createEmptyClassInstanceForUser $classAndPropURI")
    createEmptyClassInstance(newURI, classAndPropURI._1, userURI)
  }

  /** NON transactional */
  private def createEmptyClassInstance(subjectURI: Rdf#URI, classURI: Rdf#URI,
    graphURI: Rdf#URI) = {
    println(s"create Empty Class $classURI Instance for subject URI $subjectURI")
    val graph = makeGraph(List(
      makeTriple(subjectURI, rdf.typ, classURI)))
    // TODO call appendToGraph only once
    dataset.appendToGraph(graphURI, graph)
  }
}
