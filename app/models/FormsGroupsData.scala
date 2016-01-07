package models

import org.w3.banana.RDF
import org.w3.banana.RDFOps

trait FormsGroupsData {
  /**
   * values for arguments to applicationClassesAndProperties(formGroup: String)
   *  TODO should be read from RDF database
   */
  lazy val formsGroups = List("risk", "capital")

  def formGroupList(userOption: Option[User]): Map[String, Option[String]]
}

trait FormsGroupsData1[Rdf <: RDF] extends FormsGroupsData with Prefixes[Rdf] /*with ResponseAnalysis*/{

  implicit val ops: RDFOps[Rdf]
  import ops._

  /** Map forms Groups plain names to their URI */
  lazy val formsGroupsURIMap: Map[String, String] = formsGroups map {
    fgName => fgName -> fromUri(bizinnovQuestionsVocabPrefix(fgName + "-fg"))
  } toMap

  /** Map forms Groups labels to their URI */
  def formGroupList(userOption: Option[User]): Map[String, Option[String]] = Map(
    "Pré-diagnostic" -> Some(formsGroupsURIMap("risk")),
    "Diagnostic" -> { userOption match {
      case Some(user) => if(/*this.globalEval(user) > 3*/true) Some(formsGroupsURIMap("capital")) else None
      case None => None
    }}
  )
}