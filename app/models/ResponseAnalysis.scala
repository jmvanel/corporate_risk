package models

import org.apache.log4j.Logger
import org.w3.banana.Prefix
import org.w3.banana.RDF
import org.w3.banana.RDFSPrefix

import deductions.runtime.abstract_syntax.InstanceLabelsInferenceMemory
import scalax.chart.Chart
import deductions.runtime.services.SPARQLHelpers

/**
 * Responses Analysis:
 *  see "Note on the data model" in README.md
 */
trait ResponseAnalysisTrait[Rdf <: RDF, DATASET]
    extends UserDataTrait[Rdf, DATASET]
    with InstanceLabelsInferenceMemory[Rdf, DATASET]
    with SPARQLHelpers[Rdf, DATASET]
    with ResponseAnalysisOnlyInterface {

  import ops._
  import rdfStore.transactorSyntax._
  import rdfStore.graphStoreSyntax._
  import rdfStore.sparqlEngineSyntax._

  //////// Response count ////////

  /**
   * @return a list of forms, each with Label and Counts (responses, fields), for given form group
   *  @param groupUri URI of form group
   */
  def getFormsURIsLabelCounts(groupUri: String, user: User): Seq[(String, String, Int, Int)] = {
    getUserData(user, groupUri).map {
      case FormUserData(formUri, label, _) =>
        (fromUri(formUri), label,
          responsesCount(user, fromUri(formUri)),
          fieldsCount(user, fromUri(formUri)))
    }
  }

  /**
   * fonction qui compte les réponses pour une propriété de User,
   *  c'est à dire un formulaire, alias une rubrique (alias thème);
   *
   * En termes RDF, compte le nombre de triplets dont le sujet est l'URI donné.
   * transactional
   */
  def responsesCount(user: User, dataURI: String): Int = {
    val countTry = rdfStore.r( dataset, {
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
        logInfo(s""" responsesCount iter ${row}""")
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
   * En termes OWL, on compte le nombre de propriétés dont le domaine est la classe donnée.
   * transactional
   */
  def fieldsCount(user: User, classURI: String): Int = {
    val userURI = getURI(user)
    val queryString = s"""
        ${declareSPARQL_PREFIX(rdfs)}
        SELECT DISTINCT (COUNT(?PROP) AS ?count) 
        WHERE {
         GRAPH <$userURI> {
           <$classURI> a ?CLASS .
         }
         GRAPH ?ONTO {
           ?PROP rdfs:domain ?CLASS .
         }
        } """
    //    Logger.getRootLogger().debug( s"fieldsCount: $queryString")
    val countTry = rdfStore.r( dataset, {
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
    rdfStore.r( dataset, {
      Logger.getRootLogger().debug(s"getEval($userEmail, $formGroupName) userData $userData")
      val res = userData.map {
        case FormUserData(formUri, label, _) =>
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

  /** average Per Form; transactional */
  def averagePerFormTR(
    user: User,
    instanceURI: String): (Float, Int, String, Int) = {
    val res = wrapInReadTransaction {
      averagePerForm(user, instanceURI)
    }
    res . getOrElse( ( 0.0f, 0, s"Error: $res", 0 ) )
  }

  /** average Per Form; NON-transactional
   *  @return (weightedAverage, coefSum, labelClass, responseCount) */
  def averagePerForm(
    user: User,
    instanceURI: String): (Float, Int, String, Int) = {
    val userURI = getURI(user)
    val queryString = s"""
          PREFIX : <${bizinnovQuestionsVocabPrefix.prefixIri}>      
          ${declareSPARQL_PREFIX(xsd)}
          # PREFIX rdfs: <${rdfs.prefixIri}> # workaround bug on Banana: rdfs bad prefix
          ${declareSPARQL_PREFIX(rdfs)}
          ${declareSPARQL_PREFIX(bizinnovQuestionsVocabPrefix)}

          SELECT ?label (xsd:integer(?VALUE) AS ?note) (IF(bound(?COEF), xsd:integer(?COEF), 1) AS ?coef)
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
          }
          """
    import sparqlOps._
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
    var responseCount = 0
    for (tuple <- res) yield {
      val (label, note, coef) = tuple
      weightedSum += note * coef
      coefSum += coef
      labelClass = label
      responseCount = responseCount + 1
    }
    val weightedAverage = if (coefSum != 0) weightedSum / coefSum else weightedSum
    (weightedAverage, coefSum, labelClass, responseCount)
  }

  /**
   * Statistics and average Per FormGroup;
   * pour le rapport, fonction qui chiffre chaque groupe de formulaires;
   *  renvoie aussi la somme des coefficients afin de calculer la moyenne globale.
   *
   * transactional
   */
  def averagePerFormGroup(user: User, formGroupURI: String): (Float, Int, Int) = {
    val userEmail = user.email
    Logger.getRootLogger().debug(s"averagePerFormGroup($userEmail, $formGroupURI $formGroupURI)")
    val userData = getUserData(user, formGroupURI)
    val avgs = rdfStore.r( dataset, {
      val res = userData.map {
        case FormUserData(formUri, label, _) =>
          Logger.getRootLogger().debug(s"averagePerFormGroup($userEmail, $formUri) $label")
          averagePerForm(user, fromUri(formUri))
      }
      res
    }).get
    var weightedSum: Float = 0
    var coefSum = 0
    var totalCount = 0
    for (tuple <- avgs) yield {
      val (note, coef, _, count) = tuple
      weightedSum += note * coef
      coefSum += coef
      totalCount += count
    }
    val weightedAverage = if (coefSum != 0) weightedSum / coefSum else weightedSum
    (weightedAverage, coefSum, totalCount)
  }

  /**
   * fonction qui fournit la note globale
   * ( pris en compte les choix multiples pour les transformer en nombre entre 1 et 5 )
   *
   * NON transactional
   * @return (global_average, totalCount)
   */
  def globalEval(user: User): (Float, Int) = {
    //    rdfStore.r( dataset, {
    var weightedSum: Float = 0
    var coefSumGlobal = 0
    var totalCount = 0
    for (fg <- formsGroupsURIMap.values) {
      val (av, coefSum, count) = averagePerFormGroup(user, fg)
      weightedSum += av * coefSum
      coefSumGlobal += coefSum
      totalCount += count
    }
    coefSumGlobal = if (coefSumGlobal != 0) coefSumGlobal else 1
    (weightedSum / coefSumGlobal, totalCount)
    //    }).get
  }
  
  /** Map forms Groups labels to their URI;
   * and filters results according to a criterium, here
   *           globalEval(user) > 3
   * */
  def formGroupList(userOption: Option[User]): Map[String, Option[String]] = Map(
    "Pré-diagnostic" -> Some(formsGroupsURIMap("risk")),
    "Diagnostic" -> { userOption match {
      case Some(user) => if(
          globalEval(user)._1 > 3
          ) Some(formsGroupsURIMap("capital")) else None
      case None => None
    }}
  )
}


/** Interface for report generation */
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

/** Interface for form group view */
trait ResponseAnalysisOnlyInterface extends FormsGroupsData {
  def responsesCount(user: User, dataURI: String): Int
  def fieldsCount(user: User, dataURI: String): Int
  def questionsCount(): Int
  def formsCount(): Int

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
  def averagePerForm(user: User, instanceURI: String): (Float, Int, String, Int)
  def averagePerFormTR(user: User, instanceURI: String): (Float, Int, String, Int)
  def averagePerFormGroup(user: User, formGroupURI: String): (Float, Int, Int)
  def globalEval(user: User): (Float, Int)
  type DataMatch = (String, String)
}

