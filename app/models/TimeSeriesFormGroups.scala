package models

import deductions.runtime.semlogs._
import org.w3.banana.RDF
import deductions.runtime.dataset.RDFStoreLocalProvider
import java.util.Date
import deductions.runtime.semlogs.TimeSeries
import scala.concurrent.Future
import org.w3.banana.syntax._
import org.w3.banana.RDFSPrefix

/** record history of averages Per user Form */
trait TimeSeriesFormGroups[Rdf <: RDF, DATASET]
    extends TimeSeries[Rdf, DATASET]
    with ResponseAnalysisTrait[Rdf, DATASET]
    with LogAPI[Rdf] {

  import ops._
  import rdfStore.transactorSyntax._
  import rdfStore.graphStoreSyntax._
  import rdfStore.sparqlEngineSyntax._
  import scala.concurrent.ExecutionContext.Implicits.global

  private val rdfs = RDFSPrefix[Rdf]

  /**
   * save averages for current form to a specific new named graph,
   *  and add timestamp metadata to default unamed graph;
   * transactional
   */
  override def notifyDataEvent(
    addedTriples: Seq[Rdf#Triple],
    removedTriples: Seq[Rdf#Triple])(implicit userURI: String) = {
    println("TimeSeriesFormGroups.notifyDataEvent userURI " + userURI + " - " + addedTriples)
    if (!addedTriples.isEmpty)
//      Future {
        dataset2.rw({
          val (graphUri, metadata) = makeGraphURIAndMetadata(addedTriples, removedTriples)
          println("TimeSeriesFormGroups.notifyDataEvent: metadata" + metadata)
          dataset2.appendToGraph(URI(""), metadata)
          // typically 1 subject
          val subjects = addedTriples.map { _.subject }.distinct
          /* NOTE: transaction within transaction, but with a different database! */
          val graphs =
//            dataset.r({
            subjects.map { subj =>
              val avTuple = averagePerForm(User.getUserFromURI(userURI), subj.toString())
              (URI(userURI)
                -- URI("urn:average") ->- avTuple._1.toDouble
                -- rdfs.label ->- avTuple._3).graph
            }
//          }).get
          graphs.map { graph =>
            println("TimeSeriesFormGroups.notifyDataEvent: data graph" + graph)
            dataset2.appendToGraph(graphUri, graph)
          }
        })
//      }
    Unit
  }

}