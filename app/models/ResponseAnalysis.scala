package models

import org.w3.banana.jena.JenaModule
import org.w3.banana.RDFOpsModule
import deductions.runtime.jena.RDFStoreLocalProvider
import deductions.runtime.jena.RDFStoreLocalJena1Provider
import deductions.runtime.jena.RDFStoreLocalProvider2
import org.w3.banana.RDF
import org.w3.banana.jena.Jena
import com.hp.hpl.jena.query.Dataset
import deductions.runtime.jena.RDFStoreObject
import deductions.runtime.abstract_syntax.UnfilledFormFactory
import deductions.runtime.abstract_syntax.InstanceLabelsInference2
import org.w3.banana.SparqlGraphModule
import org.w3.banana.SparqlOpsModule
import org.w3.banana.diesel._
import org.apache.log4j.Logger
import java.nio.file.StandardOpenOption
import scala.xml.Elem

/** Responses Analysis */
object ResponseAnalysis extends RDFStoreLocalJena1Provider with ResponseAnalysisTrait[Jena, Dataset]

trait ResponseAnalysisTrait[Rdf <: RDF, DATASET] extends UserVocab
    with RDFStoreLocalProvider2[Rdf, DATASET]
    with InstanceLabelsInference2[Rdf]
    with SparqlGraphModule
    with SparqlOpsModule {

  /**
   * fonction qui compte les réponses pour une propriété de User,
   *  c'est à dire un formulaire, alias une rubrique (alias thème)
   */
  def responsesCount(propURI: String): Int = 0

  /**
   * pour diagramme araignée, fonction qui chiffre chaque rubrique;
   *  renvoie aussi la somme des coefficients
   */
  def averagePerForm(propURI: String): (Int, Int) = (0, 1)

  /**fonction qui fournit un rapport en HTML */
  def report(): Elem = <p>Rapport de synthèse</p>
}