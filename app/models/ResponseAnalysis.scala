package models

import scala.xml.Elem
import org.w3.banana.RDF
import org.w3.banana.SparqlGraphModule
import org.w3.banana.SparqlOpsModule
import org.w3.banana.XSDPrefix
import org.w3.banana.diesel._
import org.w3.banana.jena.Jena
import com.hp.hpl.jena.query.Dataset
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.dataset.RDFStoreLocalProvider
import org.apache.log4j.Logger
import org.w3.banana.Prefix
import org.w3.banana.RDFSPrefix
import deductions.runtime.jena.JenaRDFLoader
import deductions.runtime.abstract_syntax.InstanceLabelsInferenceMemory
import scalax.chart.Chart

/**
 * Responses Analysis:
 *  see "Note on the data model" in README.md
 */
//class ResponseAnalysis extends RDFStoreLocalJena1Provider
//  with ReportGenerationTrait[Jena, Dataset]
//  with JenaRDFLoader

trait ResponseAnalysisTrait[Rdf <: RDF, DATASET]
    extends UserDataTrait[Rdf, DATASET]
    with InstanceLabelsInferenceMemory[Rdf, DATASET]
    with ResponseAnalysisOnlyInterface {

  import ops._

  //  val xsd = XSDPrefix[Rdf]
  private val rdfs = RDFSPrefix[Rdf]

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
      println("responsesCount " + queryString)
      val query = parseSelect(queryString).get
      val solutions = dataset.executeSelect(query, Map()).get
      val res = solutions.iterator map { row =>
        infor(s""" responsesCount iter ${row}""")
        row("count").get.as[Rdf#Literal].get
      }
      res.next()
    })
    val lit = countTry.getOrElse(zero)
    println(s"responsesCount $dataURI $lit")
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
        PREFIX rdfs: <${rdfs.prefixIri}>
        SELECT DISTINCT (COUNT(?PROP) AS ?count) 
        WHERE {
         GRAPH <$userURI> {
           <$dataURI> a ?CLASS .
         }
         GRAPH ?ONTO {
           ?PROP rdfs:domain ?CLASS .
         }
        } """
    //    println(s"fieldsCount: $queryString")
    val countTry = dataset.r({
      import sparqlOps._
      val query = parseSelect(queryString).get
      val solutions = dataset.executeSelect(query, Map()).get
      val res = solutions.iterator map { row =>
        //        info(s""" fieldsCount iter ${row}""")
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
   * transactional
   */
  def getRiskEval(userEmail: String): Map[String, Float] = {
    getEvaluation(userEmail, "risk")
  }

  /**
   * renvoie la liste des formulaires de l'utilisateur avec
   * la moyenne de chacun, pour le groupe "capital"
   * transactional
   */
  def getCapitalEval(userEmail: String): Map[String, Float] = {
    getEvaluation(userEmail, "capital")
  }

  /**
   * @return Map with key from label, and value Evaluation 0<note<5
   * transactional
   */
  def getEvaluation(userEmail: String, formGroupName: String): Map[String, Float] = {
    Logger.getRootLogger().info(s"getEval($userEmail, $formGroupName)")
    val userData = getUserData(user(userEmail), formsGroupsURIMap(formGroupName))
    dataset.r({
      Logger.getRootLogger().info(s"getEval($userEmail, $formGroupName) userData $userData")
      val res = userData.map {
        case FormUserData(formUri, label) =>
          Logger.getRootLogger().info(s"getEval($userEmail, $formGroupName) $label")
          label -> averagePerForm(user(userEmail), fromUri(formUri))._1
      }
      val string2Int = res.toMap
      string2Int.map { case (s, i) => (s, i.toFloat) }
    }).get
  }

  def user(userEmail: String) = User(userEmail, "", "")

  /**
   * average per form;
   * pour diagramme araignée, fonction qui chiffre chaque rubrique;
   *  renvoie aussi la somme des coefficients afin de calculer la moyenne globale.
   *
   * NON transactional
   */
  def averagePerForm(
    user: User,
    instanceURI: String): (Float, Int, String) = {
    val userURI = getURI(user)
    val queryString = s"""
          PREFIX : <http://www.bizinnov.com/ontologies/quest.owl.ttl#>      
          ${declareSPARQL_PREFIX(xsd)}
          # ${declareSPARQL_PREFIX(rdfs)}
          PREFIX rdfs: <${rdfs.prefixIri}>
          PREFIX ques: <http://www.bizinnov.com/ontologies/quest.owl.ttl#> 

          SELECT ?label (xsd:integer(?VALUE) AS ?note) (IF(bound(?COEF), xsd:integer(?COEF) ,1) AS ?coef)
          WHERE {
           { # for direct numeric values (risk forms)
            GRAPH <$userURI> {
             <$instanceURI> ?PROP ?VALUE ;
                            a ?CLASS .
            }
            GRAPH ?ONTO {
             ?PROP :coef ?COEF .
             ?CLASS rdfs:label ?label .
            }
           } UNION { # for indirect numeric values: enumerated values have a ques:value (capital forms)
            GRAPH <$userURI> {
             <$instanceURI> ?PROP ?OBJ ;
                            a ?CLASS  .
            }
            GRAPH ?ONTO {
             ?OBJ ques:value ?VALUE .
             ?CLASS rdfs:label ?label .
            }
           }
          } """
    import sparqlOps._
    import ops._
    //    println(s"averagePerForm queryString $queryString")
    val query = parseSelect(queryString).get
    val solutions = dataset.executeSelect(query, Map()).get
    val solutionsSeq = solutions.iterator.to[List]
    Logger.getRootLogger().info(s"""averagePerForm($instanceURI)
      size ${solutionsSeq.size}
      """)
    val res = solutionsSeq map { row =>
      val sol =
        (lit2String(row("label").get.as[Rdf#Literal].get),
          lit2Int(row("note").get.as[Rdf#Literal].get),
          lit2Int(row("coef").get.as[Rdf#Literal].get))
      Logger.getRootLogger().info(s"""averagePerForm($instanceURI) solution $sol""")
      sol
    }
    var weightedSum: Float = 0
    var coefSum = 0
    var labelClass = ""
    for (tuple <- res) yield {
      val (label, note, coef) = tuple
      weightedSum += note * coef
      coefSum += coef
      labelClass = label
    }
    val weightedAverage = if (coefSum != 0) weightedSum / coefSum else weightedSum
    (weightedAverage, coefSum, labelClass)
  }

  protected def lit2Int(lit: Rdf#Literal) = ops.fromLiteral(lit)._1.toInt
  protected def lit2String(lit: Rdf#Literal) = ops.fromLiteral(lit)._1

  /**
   * average Per FormGroup;
   * pour le rapport, fonction qui chiffre chaque groupe de formulaires;
   *  renvoie aussi la somme des coefficients afin de calculer la moyenne globale.
   *
   * transactional
   */
  def averagePerFormGroup(user: User, formGroupURI: String): (Float, Int) = {
    val userEmail = user.email
    Logger.getRootLogger().info(s"averagePerFormGroup($userEmail, $formGroupURI $formGroupURI)")
    val userData = getUserData(user, formGroupURI)
    val avgs = dataset.r({
      val res = userData.map {
        case FormUserData(formUri, label) =>
          Logger.getRootLogger().info(s"averagePerFormGroup($userEmail, $formUri) $label")
          averagePerForm(user, fromUri(formUri))
      }
      res
    }).get
    var weightedSum: Float = 0
    var coefSum = 0
    for (tuple <- avgs) yield {
      val (note, coef, _) = tuple
      weightedSum += note * coef
      coefSum += coef
    }
    val weightedAverage = if (coefSum != 0) weightedSum / coefSum else weightedSum
    (weightedAverage, coefSum)
  }

  /**
   * fonction qui fournit la note globale
   * ( pris en compte les choix multiples pour les transformer en nombre entre 1 et 5 )
   *
   * transactional
   */
  def globalEval(user: User): Float = {
    //    dataset.r({
    var weightedSum: Float = 0
    var coefSumGlobal = 0
    for (fg <- formsGroupsURIMap.values) {
      val (av, coefSum) = averagePerFormGroup(user, fg)
      weightedSum += av * coefSum
      coefSumGlobal += coefSum
    }
    coefSumGlobal = if (coefSumGlobal != 0) coefSumGlobal else 1
    (weightedSum / coefSumGlobal)
    //    }).get
  }
  
  /** Map forms Groups labels to their URI;
   * and filters results acording to a criterium, here
   *           globalEval(user) > 3
   * */
  def formGroupList(userOption: Option[User]): Map[String, Option[String]] = Map(
    "Pré-diagnostic" -> Some(formsGroupsURIMap("risk")),
    "Diagnostic" -> { userOption match {
      case Some(user) => if(
          globalEval(user) > 3
          ) Some(formsGroupsURIMap("capital")) else None
      case None => None
    }}
  )
}

//trait ResponseAnalysisInterface[Rdf <: RDF] extends ResponseAnalysisInterface0 with FormsGroupsData[Rdf]

/** actually Interface for the view */
trait ResponseAnalysisInterface extends ResponseAnalysisOnlyInterface {
  def filterQuestionnaires(user: User, groupUri: String): (Seq[DataMatch] /*Good*/ ,
      Seq[DataMatch] /*Good*/ )
  /**
   * @return all non empty X-Y Charts with X = timestamp, and Y = average,
   * for all form groups,
   */
  def computeAllXYChart(email: String): Iterable[Chart]
  
  def getInfo(user: User): Option[UserCompanyInfo]
}
  
trait ResponseAnalysisOnlyInterface extends FormsGroupsData {
  def responsesCount(user: User, dataURI: String): Int
  def fieldsCount(user: User, dataURI: String): Int
  def getRiskEval(userEmail: String): Map[String, Float]
  def getCapitalEval(userEmail: String): Map[String, Float]
  def getEvaluation(userEmail: String, formGroupName: String): Map[String, Float]
  def averagePerForm(
    user: User,
    instanceURI: String): (Float, Int, String)
  def averagePerFormGroup(user: User, formGroupURI: String): (Float, Int)
  def globalEval(user: User): Float
  type DataMatch = (String, String)
}

