package views

import org.jfree.chart.StandardChartTheme
import org.jfree.chart.axis.CategoryLabelPositions
import org.w3.banana.RDF
import models.User
import scalax.chart.Chart
import scalax.chart.SpiderWebChart
import scalax.chart.api.BarChart
import scalax.chart.api.Color
import scalax.chart.api.Orientation.Vertical
import scalax.chart.api.ToCategoryDataset.FromTuple2s
import scalax.chart.api.XYLineChart
import models.ResponseAnalysisTrait
import models.TimeSeriesFormGroups
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.FixedMillisecond
import org.jfree.chart.ChartFactory
import org.jfree.data.time.TimeSeriesCollection
import org.w3.banana.binder.FromURI
import deductions.runtime.abstract_syntax.InstanceLabelsInferenceMemory
import java.awt.Color
import org.jfree.chart.plot.XYPlot
import org.jfree.ui.RectangleInsets
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.axis.DateAxis
import java.text.SimpleDateFormat
import org.jfree.chart.plot.SpiderWebPlot
import java.awt.Rectangle

trait Charts[Rdf <: RDF, DATASET]
extends ResponseAnalysisTrait[Rdf, DATASET]
  with TimeSeriesFormGroups[Rdf, DATASET]
  with InstanceLabelsInferenceMemory[Rdf, DATASET]
{
//  self: ResponseAnalysisTrait[Rdf, DATASET]
//  with TimeSeriesFormGroups[Rdf, DATASET]
//  with InstanceLabelsInferenceMemory[Rdf, DATASET] =>

  private val responseAnalysis = this;

  import ops._
  import rdfStore.transactorSyntax._
  import rdfStore.graphStoreSyntax._
  import rdfStore.sparqlEngineSyntax._
  
  /**
   * compute Chart: chart type is "risk" or "capital";
   *  transactional
   */
  def computeChart(charttype: String, email: String): Chart = {
    val user = User.find(email)
    val transparent = new Color(0xFF, 0xFF, 0xFF, 0)
    implicit val theme = new StandardChartTheme("JFree")
    theme.setChartBackgroundPaint(transparent)
    theme.setPlotBackgroundPaint(transparent)
    theme.setPlotOutlinePaint(transparent)
    val content = charttype match {
      case "risk" => 
        val chart = SpiderWebChart(responseAnalysis.getRiskEval(email).toVector)
        // FAILS:  val spiderWebPlot = chart.peer.asInstanceOf[SpiderWebPlot]  
        chart
      case "capital" => {
        val chart = BarChart(responseAnalysis.getCapitalEval(email).toVector)
        chart.plot.getDomainAxis().setCategoryLabelPositions(CategoryLabelPositions.UP_45)
        chart
      }
      case _ => throw new IllegalArgumentException("Please specify the kind of chart you want")
    }
    content
  }
  
  /**
   * @return all non empty X-Y Charts with X = timestamp, and Y = average,
   * for all form groups,
   *  associated with given user
   */
  def computeAllXYChart(email: String): Iterable[Chart] = {
    val ll = for (formGroupURI <- formsGroupsURIMap.values) yield {
      computeXYCharts(formGroupURI, email)
    }
    ll.flatten
  }

  /**
   * @return all non empty X-Y Charts with X = timestamp, and Y = average for given form group,
   *  associated with given user
   */
  def computeXYCharts(formGroupURI: String, email: String): Iterable[Chart] = {
    // TODO ? move to a class to manage data model
    val labelsInFormGroup = dataset.rw({
      val formGroup = applicationClassesAndProperties(URI(formGroupURI))
      val classesAndProperties = formGroup.classesAndProperties
      classesAndProperties.map {
        case (classe, property) =>
          instanceLabel(classe, allNamedGraph, "fr")
      }
    }).get
    println( "labelsInFormGroup " + labelsInFormGroup )
    implicit val userURI = User.getUserURIFromEmail(email)
    //    getTimeSeries(predicateURI = "urn:average")
    val timeSeries = getTimeSeries()
    println(s"timeSeries $timeSeries")
    val labelsAndTimeSeries = for {
      label <- timeSeries.keys
       // filter labels not corresponding to given formGroupURI
       if labelsInFormGroup.contains(label)
      ts1 <- timeSeries.get(label) if (!ts1.isEmpty)
    } yield {
      val ts = ts1.toIndexedSeq.map {
        e => ((e._1.longValueExact()), e._2) }
      println(">>> computeXYCharts " + ts)
      label -> ts
    }
    val chartDataset = new TimeSeriesCollection()
    for( (label, timeSeries) <- labelsAndTimeSeries ) {
      val tsForJFreeCharts = new TimeSeries( label )
      timeSeries . map {
        case (timestamp, value) =>
        tsForJFreeCharts.add(new FixedMillisecond(timestamp), value)
      }
      chartDataset.addSeries(tsForJFreeCharts)
    }
      // inspired by org.jfree.chart.demo.TimeSeriesChartDemo1
      val chart = ChartFactory.createTimeSeriesChart(
        formGroupURIToLabel(formGroupURI), // title
        "Date", // x-axis label
        "", // y-axis label
        chartDataset,
        true, // create legend?
        true, // generate tooltips?
        false // generate URLs?
        );
      //      chart.show()  // debug <<
      configureXYChart(chart)
      Seq(Chart.fromPeer(chart))
  }

  private def configureXYChart(chart: org.jfree.chart.JFreeChart) {
    chart.setBackgroundPaint(Color.white);
    val plot = chart.getPlot().asInstanceOf[XYPlot]
    plot.setBackgroundPaint(Color.lightGray);
    plot.setDomainGridlinePaint(Color.white);
    plot.setRangeGridlinePaint(Color.white);
    plot.setAxisOffset(new RectangleInsets(5.0, 5.0, 5.0, 5.0));
    plot.setDomainCrosshairVisible(true);
    plot.setRangeCrosshairVisible(true);
    val r = plot.getRenderer()
    r match {
      case renderer: XYLineAndShapeRenderer =>
        renderer.setBaseShapesVisible(true);
        renderer.setBaseShapesFilled(true);
        renderer.setDrawSeriesLineAsPath(true);
    }
    val timeAxis = plot.getDomainAxis().asInstanceOf[DateAxis]
    timeAxis.setDateFormatOverride(new SimpleDateFormat("MMM-yyyy"))

    val valueAxis = plot.getRangeAxis.setRange( 0, 5)
  }
}
