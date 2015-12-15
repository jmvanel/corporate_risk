package views.charts

import org.jfree.chart.StandardChartTheme
import org.jfree.chart.axis.CategoryLabelPositions
import models.ResponseAnalysis
import models.User
import scalax.chart.SpiderWebChart
import scalax.chart.api.BarChart
import scalax.chart.api.Color
import org.w3.banana.RDF
import models.ResponseAnalysisTrait

trait Charts[Rdf <: RDF, DATASET] {
  self: ResponseAnalysisTrait[Rdf, DATASET] =>

  //  val responseAnalysis = new ResponseAnalysis()
  private val responseAnalysis = this;

  /** compute Chart: chart type is "risk" or "capital" */
  def computeChart(charttype: String, email: String) = {
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
}