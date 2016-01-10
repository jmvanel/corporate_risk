package views

import org.jfree.chart.StandardChartTheme
import org.jfree.chart.axis.CategoryLabelPositions
import scalax.chart.SpiderWebChart
import scalax.chart.api.BarChart
import scalax.chart.api.XYLineChart
import scalax.chart.api.Color
import scalax.chart.api.Orientation._
import org.w3.banana.RDF
import models.User
import models.ResponseAnalysisTrait
import models.TimeSeriesFormGroups
import scalaz.IsEmpty
import scalax.chart.XYChart
import scalax.chart.Chart

trait Charts[Rdf <: RDF, DATASET] {
  self: ResponseAnalysisTrait[Rdf, DATASET] with TimeSeriesFormGroups[Rdf, DATASET] =>

  private val responseAnalysis = this;

  /** compute Chart: chart type is "risk" or "capital";
   *  transactional */
  def computeChart(charttype: String, email: String): Chart = {
    val user = User.find(email)
    val transparent = new Color(0xFF, 0xFF, 0xFF, 0)
    val theme = new StandardChartTheme("JFree")
    theme.setChartBackgroundPaint(transparent)
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
    implicit val userURI = email
    //    getTimeSeries(predicateURI = "urn:average")
    //    getTimeSeries("urn:average")
    val timeSeries = getTimeSeries()
    println("timeSeries " + timeSeries)
    for {
      label <- timeSeries.keys
      ts1 <- timeSeries.get(label) if (!ts1.isEmpty)
    } yield {
      val ts // : scala.collection.immutable.IndexedSeq[(java.util.Date, Double)]
      = ts1.toIndexedSeq.map {
        e =>
          (
            // new java.util.Date
            (e._1.longValueExact()), e._2)
      }
      val chart = XYLineChart(data = ts, title = label, orientation = Vertical, legend = true)
      //      chart.show()  // debug <<
      chart
    }
  }
}