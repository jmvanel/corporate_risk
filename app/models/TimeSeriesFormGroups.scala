package models

import scala.concurrent.ExecutionContext.Implicits

import org.w3.banana.RDF
import org.w3.banana.RDFSPrefix

import deductions.runtime.semlogs.LogAPI
import deductions.runtime.semlogs.TimeSeries

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

  /**
   * save averages for current form to a specific new named graph,
   *  and add timestamp metadata to metadata graph;
   *
   * Data saved is used in web service Application#history(),
   * calling Charts#computeAllXYChart() ,
   * calling  TimeSeries#getTimeSeries() from semantic_forms ;
   * transactional
   */
  override def notifyDataEvent(
    addedTriples: Seq[Rdf#Triple],
    removedTriples: Seq[Rdf#Triple],
    ipAdress: String,isCreation: Boolean)(implicit userURI: String) = {
    println("TimeSeriesFormGroups.notifyDataEvent userURI " + userURI + " - " + addedTriples)
    if (!addedTriples.isEmpty)
//      Future {
        dataset2.rw({
          val (graphUri, metadata) = makeGraphURIAndMetadata(addedTriples, removedTriples)
          println("TimeSeriesFormGroups.notifyDataEvent: metadata" + metadata)
          dataset2.appendToGraph( metadataGraph, metadata)
          // typically 1 subject
          val subjects = addedTriples.map { _.subject }.distinct
          /* NOTE: transaction within transaction, but with a different database! */
          val graphs =
//            rdfStore.r( dataset, {
            subjects.map { subject =>
              val avTuple = averagePerForm(User.getUserFromURI(userURI), subject.toString())
              ( subject // URI(userURI)
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
