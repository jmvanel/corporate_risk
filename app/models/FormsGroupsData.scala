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

/** Get the list of form group URI's:
 *  formGroupListRaw.values */
trait FormsGroupsData1[Rdf <: RDF] extends FormsGroupsData with Prefixes[Rdf] {

  implicit val ops: RDFOps[Rdf]
  import ops._

  /** Map forms Groups plain names (eg "risk", "capital") to their URI */
  lazy val formsGroupsURIMap: Map[String, String] = formsGroups map {
    fgName => fgName -> fromUri(bizinnovQuestionsVocabPrefix(fgName + "-fg"))
  } toMap

  /** Map forms Groups labels to their URI */
  lazy val formGroupListRaw : Map[String, String] = Map(
    "Pré-diagnostic" -> formsGroupsURIMap("risk"),
    "Diagnostic" -> formsGroupsURIMap("capital") )

   /** the reverse to formGroupListRaw: map URI to form Group labels */
  lazy val formGroupURIToLabel = formGroupListRaw.map {_.swap}
}
