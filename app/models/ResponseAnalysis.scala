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
import org.w3.banana.Prefix

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
  import rdfStore.transactorSyntax._
  import rdfStore.graphStoreSyntax._
  import rdfStore.sparqlEngineSyntax._

  //////// Response count ////////

  /**
   * fonction qui compte les réponses pour une propriété de User,
   *  c'est à dire un formulaire, alias une rubrique (alias thème);
   *
   *  transactional
   */
  def responsesCount(user: User, dataURI: String): Int = {
    val countTry = dataset.r({
      val userURI = getURI(user)
      // NOTE: could have used find() like in UserData.getUserData()
      val queryString = s"""
          ${declareSPARQL_PREFIX(rdf)}
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
      val solutions = dataset.executeSelect(query, Map()).get
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
   * fonction qui compte le nombre de champs pour une propriété de User,
   *  c'est à dire un formulaire, alias une rubrique (alias thème);
   *
   *  transactional
   */
  def fieldsCount(user: User, dataURI: String): Int = {
    val userURI = getURI(user)
    val queryString = s"""
        PREFIX rdfs: <${rdfs.prefixIri}>"
        SELECT DISTINCT (COUNT(?PROP) AS ?count) 
        WHERE {
         GRAPH <$userURI> {
           <$dataURI> a ?CLASS .
         }
         GRAPH ?ONTO {
           ?PROP rdfs:domain ?CLASS .
         }
        } """
    println(s"fieldsCount: $queryString")
    val countTry = dataset.r({
      import sparqlOps._
      val query = parseSelect(queryString).get
      val solutions = dataset.executeSelect(query, Map()).get
      val res = solutions.iterator map { row =>
        row("count").get.as[Rdf#Literal].get
      }
      res.next()
    })
    val lit = countTry.getOrElse(zero)
    println("fieldsCount " + lit)
    lit2Int(lit)
  }

  def declareSPARQL_PREFIX(pr: Prefix[_]) = {
    s"PREFIX ${pr.prefixName}: <${pr.prefixIri}>"
  }

  //////// Response Analysis proper ////////

  /**
   * fonction qui renvoie la liste des formulaires de l'utilisateur avec
   * la moyenne de chacun,  pour le groupe "risk".
   */
  def getRiskEval(userEmail: String): Map[String, Double] = {
    getEvaluation(userEmail, "risk")
  }

  /**
   * renvoie la liste des formulaires de l'utilisateur avec
   * la moyenne de chacun, pour le groupe "capital"
   */
  def getCapitalEval(userEmail: String): Map[String, Double] = {
    getEvaluation(userEmail, "capital")
    // Map( "Capital humain" -> 3.5,
    //      "Capital naturel" -> 2,
    //      "Capital marques" -> 4 )
  }

  /** transactional */
  def getEvaluation(userEmail: String, formGroupName: String): Map[String, Double] = {
    Logger.getRootLogger().info(s"getEval($userEmail, $formGroupName)")
    val userData = getUserData(user(userEmail), bizinnovQuestionsVocabPrefix(formGroupName).toString)
    dataset.r({
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
    user: User,
    instanceURI: String): (Int, Int) = {
    val userURI = getURI(user)
    val queryString = s"""
          PREFIX : <http://www.bizinnov.com/ontologies/quest.owl.ttl#>      
          ${declareSPARQL_PREFIX(xsd)}
          # ${declareSPARQL_PREFIX(rdfs)}
          PREFIX rdfs: <${rdfs.prefixIri}>
          PREFIX ques: <http://www.bizinnov.com/ontologies/quest.owl.ttl#> 

          SELECT ?label (xsd:integer(?VALUE) AS ?note) (xsd:integer(?COEF) AS ?coef)
          WHERE {
           {
            GRAPH <$userURI> {
             <$instanceURI> ?PROP ?VALUE ;
                            a ?CLASS .
            }
            GRAPH ?ONTO {
             ?PROP :coef ?COEF .
             ?CLASS rdfs:label ?label .
            }
           } UNION {
            GRAPH <$userURI> {
             <$instanceURI> ?PROP ?OBJ .
            }
            GRAPH ?ONTO {
             ?OBJ ques:value ?VALUE
            }
           }
          } """
    import sparqlOps._
    import ops._
    //    println(queryString)
    val query = parseSelect(queryString).get
    val solutions = dataset.executeSelect(query, Map()).get
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
  }

  protected def lit2Int(lit: Rdf#Literal) = ops.fromLiteral(lit)._1.toInt
  protected def lit2String(lit: Rdf#Literal) = ops.fromLiteral(lit)._1

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
    dataset.r({
      var weightedSum = 0
      var coefSumGlobal = 0
      for (
        fg <- formsGroupsURIMap.values // .map()
      ) {
        //        val (av, coefSum) = averagePerFormGroup(user, ops.fromUri(fg))
        val (av, coefSum) = averagePerFormGroup(user, fg)
        weightedSum += av * coefSum
        coefSumGlobal += coefSum
      }
      coefSumGlobal = if (coefSumGlobal != 0) coefSumGlobal else 1
      (weightedSum / coefSumGlobal)
    }).get
  }

}