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


  /**
   * fonction qui compte les réponses pour une propriété de User,
   *  c'est à dire un formulaire, alias une rubrique (alias thème);
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

    /**
   * fonction qui renvoie la liste des formulaires de l'utilisateur avec
   * la moyenne de chacun,  pour le groupe "risk".
   */
  def getRiskEval(userEmail: String): Map[String, Double] = {
    getEval(userEmail, "risk")
  }
  
 def getEval(userEmail: String, formGroupName: String): Map[String, Double] = {
    val riskQuestionaires = applicationClassesAndProperties(formGroupName)
    val cp = riskQuestionaires.classesAndProperties
    val res = for ( (c, propURI) <- cp ) yield {
        implicit val graph = allNamedGraph
        instanceLabel(propURI) ->
        averagePerForm( User.find(userEmail).get, fromUri(propURI))._1 
    }
    val string2Int = res.toMap
    string2Int.map { case(s,i) => (s, i.toDouble) }    
  }
 
  /**
   * renvoie la liste des formulaires de l'utilisateur avec
   * la moyenne de chacun, pour le groupe "capital"
   */
  def getCapitalEval(userEmail: String): Map[String, Double] = {
	  getEval(userEmail, "capital")
//    Map(
//      "Capital humain" -> 3.5,
//      "Capital naturel" -> 2,
//      "Capital marques" -> 4
//    )
  }
  
  /**
   * pour diagramme araignée, fonction qui chiffre chaque rubrique;
   *  renvoie aussi la somme des coefficients afin de calculer la moyenne globale.
   */
  def averagePerForm(
//      email: String,
      user: User, 
      propURI: String
      ): (Int, Int) = {
    val iteratorTry = rdfStore.r(dataset, {
      val userURI = getURI(user)
//      bizinnovUserPrefix(email)
      // NOTE: could have used find() like in UserData.getUserData()
      val queryString = s"""
          prefix : <http://www.bizinnov.com/ontologies/quest.owl.ttl#>
          SELECT ?label (xsd:integer(?OBJ) AS ?note) (xsd:integer(?COEF) AS ?coef)
          WHERE {
           GRAPH <$userURI> {
             <$propURI> ?PROP ?OBJ .
           }
           GRAPH ?any {
             ?PROP :coef ?COEF ;
             rdfs:label ?LAB .
           }
          } """
      import sparqlOps._
      import ops._
      val query = parseSelect(queryString).get
      val solutions = rdfStore.executeSelect(dataset, query, Map()).get
      val res = solutions.iterator map { row =>
        (lit2String(row("label").get.as[Rdf#Literal].get),
          lit2Int(row("note").get.as[Rdf#Literal].get),
          lit2Int(row("coef").get.as[Rdf#Literal].get))
      }
      var weightedSum = 0
      var coefSum = 0
      for (tuple <- res) yield {
        val (label, note, coef) = tuple
        weightedSum += note * coef
        coefSum += coef
      }
      val weightedAverage = weightedSum / coefSum
      (weightedAverage, coefSum)
    })
    iteratorTry.getOrElse(0, 1)
  }

  private def lit2Int(lit: Rdf#Literal) = ops.fromLiteral(lit)._1.toInt
  private def lit2String(lit: Rdf#Literal) = ops.fromLiteral(lit)._1

  /**
   * pour le rapport, fonction qui chiffre chaque groupe de formulaires;
   *  renvoie aussi la somme des coefficients afin de calculer la moyenne globale.
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
    val weightedAverage = weightedSum / coefSum
    (weightedAverage, coefSum)
  }

  /**
   * fonction qui fournit un rapport en HTML
   *  TODO : prendre en compte les choix multiples pour les transformer en nombre entre 1 et 5
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
      (weightedSum / coefSumGlobal)
    }).get
  }

}