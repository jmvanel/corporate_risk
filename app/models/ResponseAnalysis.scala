package models

import org.apache.log4j.Logger
import org.w3.banana.Prefix
import org.w3.banana.RDF
import org.w3.banana.RDFSPrefix

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
      Logger.getRootLogger().debug("responsesCount " + queryString)
      val query = parseSelect(queryString).get
      val solutions = dataset.executeSelect(query, Map()).get
      val res = solutions.iterator map { row =>
        infor(s""" responsesCount iter ${row}""")
        row("count").get.as[Rdf#Literal].get
      }
      res.next()
    })
    val lit = countTry.getOrElse(zero)
    Logger.getRootLogger().debug( s"responsesCount $dataURI $lit")
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
    //    Logger.getRootLogger().debug( s"fieldsCount: $queryString")
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
    Logger.getRootLogger().debug( "fieldsCount " + lit)
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
    Logger.getRootLogger().debug(s"getEval($userEmail, $formGroupName)")
    try {
    val userData = getUserData(user(userEmail), formsGroupsURIMap(formGroupName))
    dataset.r({
      Logger.getRootLogger().debug(s"getEval($userEmail, $formGroupName) userData $userData")
      val res = userData.map {
        case FormUserData(formUri, label) =>
          Logger.getRootLogger().debug(s"getEval($userEmail, $formGroupName) $label")
          label -> averagePerForm(user(userEmail), fromUri(formUri))._1
      }
      val string2Int = res.toMap
      string2Int.map { case (s, i) => (s, i.toFloat) }
    }).get
    } catch {
      case t: Throwable => t.printStackTrace()
      Map()
    }
  }

  /** get user object from her Email; convenience method that does not populate user credentials;
   *  NOTE: for that, see method: User.find(email: String) */
  def user(userEmail: String): User = User(userEmail, "", "")

  def averagePerForm(
    user: User,
    instanceURI: String): (Float, Int, String) = {
    val userURI = getURI(user)
//          # ${declareSPARQL_PREFIX(rdfs)}
    val queryString = s"""
          PREFIX : <${bizinnovQuestionsVocabPrefix.prefixIri}>      
          ${declareSPARQL_PREFIX(xsd)}
          # PREFIX rdfs: <${rdfs.prefixIri}> # workaround bug on Banana: rdfs bad prefix
          ${declareSPARQL_PREFIX(rdfs)}
          ${declareSPARQL_PREFIX(bizinnovQuestionsVocabPrefix)}

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
           } UNION {
           # for indirect numeric values: enumerated values having a ques:value (capital forms)
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
    //    Logger.getRootLogger().debug(s"averagePerForm queryString $queryString")
    val query = parseSelect(queryString).get
    val solutions = dataset.executeSelect(query, Map()).get
    val solutionsSeq = solutions.iterator.to[List]
    Logger.getRootLogger().debug(s"""averagePerForm($instanceURI)
      solutions size ${solutionsSeq.size}
      """)
    val res = solutionsSeq map { row =>
      val sol =
        (lit2String(row("label").get.as[Rdf#Literal].get),
          lit2Int(row("note").get.as[Rdf#Literal].get),
          lit2Int(row("coef").get.as[Rdf#Literal].get))
      Logger.getRootLogger().debug(s"""averagePerForm($instanceURI) solution $sol""")
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
    Logger.getRootLogger().debug(s"averagePerFormGroup($userEmail, $formGroupURI $formGroupURI)")
    val userData = getUserData(user, formGroupURI)
    val avgs = dataset.r({
      val res = userData.map {
        case FormUserData(formUri, label) =>
          Logger.getRootLogger().debug(s"averagePerFormGroup($userEmail, $formUri) $label")
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


/** actually overall Interface for the view */
trait ResponseAnalysisInterface extends ResponseAnalysisOnlyInterface {
  def filterQuestionnaires(user: User, groupUri: String): (Seq[DataMatch] /*Good*/ ,
      Seq[DataMatch] /*Good*/ )
  /**
   * @return all non empty X-Y Charts with X = timestamp, and Y = average,
   * for all form groups,
   */
  def computeAllXYChart(email: String): Iterable[Chart]
  
  def getCompanyInfo(user: User): Option[UserCompanyInfo]
  val formsGroupsURIMap: Map[String, String]
  def getNAFLabel(number: String, lang: String = "fr"): String
  def getLastUpdate(userURI: String): Option[(String, String)]
}

trait ResponseAnalysisOnlyInterface extends FormsGroupsData {
  def responsesCount(user: User, dataURI: String): Int
  def fieldsCount(user: User, dataURI: String): Int
  def getRiskEval(userEmail: String): Map[String, Float]
  def getCapitalEval(userEmail: String): Map[String, Float]
  def getEvaluation(userEmail: String, formGroupName: String): Map[String, Float]
  /**
   * average per form;
   * pour diagramme araignée, fonction qui chiffre chaque rubrique;
   *  renvoie aussi la somme des coefficients afin de calculer la moyenne globale.
   *
   * NON transactional
   */
  def averagePerForm(
    user: User,
    instanceURI: String): (Float, Int, String)
  def averagePerFormGroup(user: User, formGroupURI: String): (Float, Int)
  def globalEval(user: User): Float
  type DataMatch = (String, String)
}

