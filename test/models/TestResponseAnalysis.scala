package models

import org.scalatest.FunSuite
import org.apache.log4j.Logger
import scala.util.Success
import scala.util.Failure
import scala.reflect.io.Path
import scala.util.Try

class TestResponseAnalysis extends FunSuite {
  import UserData._
  import ops._
  import rdfStore.transactorSyntax._
  // TODO set a test TDB, to avoid destroy the default one  
  deleteLocalSPARL()
  tdb.tdbloader.main("--loc=TDB", "--graph=vocabulary",
    "vocabulary/risk/risk_questions.owl.ttl",
    "vocabulary/risk/labels.ttl"
  )
  val responseAnalysis = new ResponseAnalysis()

  val name = "zz"
  val user = User(name, name)
  createEmptyUserData(user)
  val prop = URI("http://www.bizinnov.com/ontologies/quest.owl.ttl#5-1")

  test("averagePerForm 1") {
    import UserData._
    import ops._
    val formGroupUri = UserData.formsGroupsURIMap("risk")
    val fuds = getUserData(user, formGroupUri)
    if (!fuds.isEmpty) {
      val fud = fuds(0)
      val instanceURI = fud.data
      responseAnalysis.dataset.rw({
        val graph = (
          instanceURI -- prop ->- "2"
          -- prop ->- 3
          -- prop ->- "5"
          a URI("http://www.bizinnov.com/ontologies/quest.owl.ttl#5")
        ).graph
        val res = responseAnalysis.rdfStore.appendToGraph(responseAnalysis.dataset, user.getURI(), graph) match {
          case Success(x) => x
          case Failure(e) => "Failure in appendToGraph " + e
        }
        info(s"after appendToGraph user.getURI() ${user.getURI()} res $res")
        val (avg, coefs) = responseAnalysis.averagePerForm(user, fromUri(instanceURI))
        info(s""" responseAnalysis ${fud.label}
          avg ${avg} coefs $coefs""")
        assert( avg == 3.3333333f )
      })
    }
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