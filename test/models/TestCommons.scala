package models

import scala.reflect.io.Path
import scala.util.Try

object TestCommons {

  /**
   * delete previousLocal SPARL;
   * load part of vocabulary/risk/ and vocabulary/capital/
   */
  def prepareTDBontologies() {
    // TODO set a test TDB, to avoid destroy the default one  
    deleteLocalSPARL()
    tdb.tdbloader.main("--loc=TDB", "--graph=vocabulary",
      "vocabulary/risk/risk_questions.owl.ttl",
      "vocabulary/risk/labels.ttl",
      "vocabulary/capital/Evaluation_du_capital_humain.owl.ttl"
    )
  }

  def info(s: String) = {
    System.out.println(s)
    // Logger.getRootLogger().error(s)
  }

  def deleteLocalSPARL() = {
    val path: Path = Path("TDB")
    Try(path.deleteRecursively()) // continueOnFailure = false))
  }
}