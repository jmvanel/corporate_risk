package models

import org.w3.banana.RDF
import org.w3.banana.RDFOps

trait FormsGroupsData {
  /**
   * values for arguments to applicationClassesAndProperties(formGroup: String)
   *  TODO should be read from RDF database
   */
  lazy val formsGroups = List("risk", "capital")

  val formGroupList: Map[String, String]
}

trait FormsGroupsData1[Rdf <: RDF] extends FormsGroupsData with Prefixes[Rdf] {

  implicit val ops: RDFOps[Rdf]
  import ops._

  /** Map forms Groups plain names to their URI */
  lazy val formsGroupsURIMap: Map[String, String] = formsGroups map {
    fgName => fgName -> fromUri(bizinnovQuestionsVocabPrefix(fgName + "-fg"))
  } toMap

  /** Map forms Groups labels to their URI */
  lazy val formGroupList: Map[String, String] = Map(
    "Pré-diagnostic" -> formsGroupsURIMap("risk"),
    "Diagnostic" -> formsGroupsURIMap("capital")
  )
}