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

trait Charts[Rdf <: RDF, DATASET] {
  self: ResponseAnalysisTrait[Rdf, DATASET] with TimeSeriesFormGroups[Rdf, DATASET] =>

  private val responseAnalysis = this;

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
      case "risk" => SpiderWebChart(responseAnalysis.getRiskEval(email).toVector)
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
    implicit val userURI = User.getUserURIFromEmail(email)
    //    getTimeSeries(predicateURI = "urn:average")
    val timeSeries = getTimeSeries()
    println(s"timeSeries $timeSeries")
    val labelsAndTimeSeries = for {
      label <- timeSeries.keys
      ts1 <- timeSeries.get(label) if (!ts1.isEmpty)
    } yield {
      val ts = ts1.toIndexedSeq.map {
        e => ((e._1.longValueExact()), e._2)
      }
      println(">>> computeXYCharts " + ts)
      label -> ts
    }

    val dataset = new TimeSeriesCollection()
    for( (label, timeSeries) <- labelsAndTimeSeries ) {
      val tsForJFreeCharts = new TimeSeries( label )
      timeSeries . map {
        case (timestamp, value) =>
        tsForJFreeCharts.add(new FixedMillisecond(timestamp), value)
      }
      dataset.addSeries(tsForJFreeCharts)
    }
      // inspired by org.jfree.chart.demo.TimeSeriesChartDemo1
      val chart = ChartFactory.createTimeSeriesChart(
        formGroupURIToLabel(formGroupURI), // title
        "Date", // x-axis label
        "", // y-axis label
        dataset,
        true, // create legend?
        true, // generate tooltips?
        false // generate URLs?
        );

      // XYLineChart(data = ts, title = label, orientation = Vertical, legend = true)
      //      chart.show()  // debug <<
    Seq(Chart.fromPeer(chart))
  }
}
