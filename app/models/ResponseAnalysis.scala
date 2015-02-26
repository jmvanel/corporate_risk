package models

import scala.xml.Elem

import org.w3.banana.RDF
import org.w3.banana.SparqlGraphModule
import org.w3.banana.SparqlOpsModule
import org.w3.banana.XSDPrefix
import org.w3.banana.diesel._
import org.w3.banana.jena.Jena

import com.hp.hpl.jena.query.Dataset

import deductions.runtime.abstract_syntax.InstanceLabelsInference2
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.dataset.RDFStoreLocalProvider

import org.apache.log4j.Logger

/**
 * Responses Analysis:
 *  see "Note on the data model" in README.md
 */
class ResponseAnalysis extends RDFStoreLocalJena1Provider with ResponseAnalysisTrait[Jena, Dataset]

trait ResponseAnalysisTrait[Rdf <: RDF, DATASET]
    extends UserDataTrait[Rdf, DATASET]
    with InstanceLabelsInference2[Rdf] {
  val xsd = XSDPrefix[Rdf]
  val zero = ops.makeLiteral("0", xsd.integer)
  import ops._

  //////// Response count ////////

  /**
   * fonction qui compte les réponses pour une propriété de User,
   *  c'est à dire un formulaire, alias une rubrique (alias thème);
   *
   *  NON transactional
   */
  def responsesCount(user: User, dataURI: String): Int = {
    val countTry = rdfStore.r(dataset, {
      val userURI = getURI(user)
      // NOTE: could have used find() like in UserData.getUserData()
      val queryString = s"""
          PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          SELECT DISTINCT (COUNT(?OBJ) AS ?count) 
          WHERE {
           GRAPH <$userURI> {
             <$dataURI> ?PROP ?OBJ .
             FILTER( ?PROP != rdf:type )
           }
          } """
      import sparqlOps._
      import ops._
      //      println("responsesCount " + queryString)
      val query = parseSelect(queryString).get
      val solutions = rdfStore.executeSelect(dataset, query, Map()).get
      val res = solutions.iterator map { row =>
        row("count").get.as[Rdf#Literal].get
      }
      res.next()
    })
    val lit = countTry.getOrElse(zero)
    println("responsesCount " + lit)
    lit2Int(lit)
  }

  //////// Response Analysis proper ////////

  /**
   * fonction qui renvoie la liste des formulaires de l'utilisateur avec
   * la moyenne de chacun,  pour le groupe "risk".
   */
  def getRiskEval(userEmail: String): Map[String, Double] = {
    getEval(userEmail, "risk")
  }

  /**
   * renvoie la liste des formulaires de l'utilisateur avec
   * la moyenne de chacun, pour le groupe "capital"
   */
  def getCapitalEval(userEmail: String): Map[String, Double] = {
    getEval(userEmail, "capital")
    // Map( "Capital humain" -> 3.5,
    //      "Capital naturel" -> 2,
    //      "Capital marques" -> 4 )
  }

  /** transactional */
  def getEval(userEmail: String, formGroupName: String): Map[String, Double] = {
    Logger.getRootLogger().info(s"getEval($userEmail, $formGroupName)")
    val userData = getUserData(user(userEmail), bizinnovQuestionsVocabPrefix(formGroupName))
    rdfStore.r(dataset, {
      Logger.getRootLogger().info(s"getEval($userEmail, $formGroupName) userData $userData")
      val res = userData.map {
        case FormUserData(formUri, label) =>
          Logger.getRootLogger().info(s"getEval($userEmail, $formGroupName) $label")
          label -> averagePerForm(user(userEmail), fromUri(formUri))._1
      }
      val string2Int = res.toMap
      string2Int.map { case (s, i) => (s, i.toDouble) }
    }).get
  }

  private def user(userEmail: String) = User(userEmail, "", "")

  /**
   * average per form;
   * pour diagramme araignée, fonction qui chiffre chaque rubrique;
   *  renvoie aussi la somme des coefficients afin de calculer la moyenne globale.
   *
   * NON transactional
   */
  def averagePerForm(
    //      email: String,
    user: User,
    instanceURI: String): (Int, Int) = {
    val userURI = getURI(user)
    //      bizinnovUserPrefix(email)
    // NOTE: could have used find() like in UserData.getUserData()
    val queryString = s"""
          prefix : <http://www.bizinnov.com/ontologies/quest.owl.ttl#>
          prefix xsd: <http://www.w3.org/2001/XMLSchema#>
          PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

          SELECT ?label (xsd:integer(?OBJ) AS ?note) (xsd:integer(?COEF) AS ?coef)
          WHERE {
           GRAPH <$userURI> {
             <$instanceURI> ?PROP ?OBJ ;
                            a ?CLASS .
           }
           GRAPH ?any {
             ?PROP :coef ?COEF .
             ?CLASS rdfs:label ?label .
           }
          } """
    import sparqlOps._
    import ops._
    val query = parseSelect(queryString).get
    val solutions = rdfStore.executeSelect(dataset, query, Map()).get
    val solutionsSeq = solutions.iterator.toSeq
    Logger.getRootLogger().info(s"""averagePerForm($instanceURI)
      size ${solutionsSeq.size}
      queryString""")
    val res = solutionsSeq map { row =>
      val sol =
        (lit2String(row("label").get.as[Rdf#Literal].get),
          lit2Int(row("note").get.as[Rdf#Literal].get),
          lit2Int(row("coef").get.as[Rdf#Literal].get))
      Logger.getRootLogger().info(s"""averagePerForm($instanceURI) solution $sol""")
      sol
    }

    var weightedSum = 0
    var coefSum = 0
    for (tuple <- res) yield {
      val (label, note, coef) = tuple
      weightedSum += note * coef
      coefSum += coef
    }
    val weightedAverage = if (coefSum != 0) weightedSum / coefSum else weightedSum
    (weightedAverage, coefSum)
    //    })
    //    iteratorTry.getOrElse(0, 1)
  }

  private def lit2Int(lit: Rdf#Literal) = ops.fromLiteral(lit)._1.toInt
  private def lit2String(lit: Rdf#Literal) = ops.fromLiteral(lit)._1

  /**
   * pour le rapport, fonction qui chiffre chaque groupe de formulaires;
   *  renvoie aussi la somme des coefficients afin de calculer la moyenne globale.
   *
   * NON transactional
   */
  def averagePerFormGroup(user: User, formGroupURI: String): (Int, Int) = {
    val fg = applicationClassesAndProperties(makeUri(formGroupURI))
    val cps = fg.classesAndProperties
    var weightedSum = 0
    var coefSum = 0
    for (cp <- cps) {
      val av = averagePerForm(user, fromUri(cp._2))
      weightedSum += av._1 * av._2
      coefSum += av._2
    }
    val weightedAverage = if (coefSum != 0) weightedSum / coefSum else weightedSum
    (weightedAverage, coefSum)
  }

  /**
   * fonction qui fournit un rapport en HTML
   *  TODO : prendre en compte les choix multiples pour les transformer en nombre entre 1 et 5
   *
   * transactional
   */
  def globalEval(user: User): Int = {
    rdfStore.r(dataset, {
      var weightedSum = 0
      var coefSumGlobal = 0
      for (fg <- formsGroupsURIs) {
        val (av, coefSum) = averagePerFormGroup(user, ops.fromUri(fg))
        weightedSum += av * coefSum
        coefSumGlobal += coefSum
      }
      coefSumGlobal = if (coefSumGlobal != 0) coefSumGlobal else 1
      (weightedSum / coefSumGlobal)
    }).get
  }

}