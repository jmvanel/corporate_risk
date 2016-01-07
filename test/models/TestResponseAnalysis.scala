package models

import org.scalatest.FunSuite
import org.apache.log4j.Logger
import scala.util.Success
import scala.util.Failure
import scala.reflect.io.Path
import scala.util.Try
import org.scalatest.BeforeAndAfterAll
import org.w3.banana.jena.Jena
import com.hp.hpl.jena.query.Dataset
import deductions.runtime.jena.RDFStoreLocalJena1Provider

class TestResponseAnalysis extends FunSuite with BeforeAndAfterAll {
  import UserData._
  import ops._
  import rdfStore.transactorSyntax._
  import TestCommons._

  lazy val name = "zz"
  lazy val user = User(name, name)
  lazy val responseAnalysis = new ResponseAnalysisTrait[Jena, Dataset] with RDFStoreLocalJena1Provider {}
  lazy val formGroupUri = UserData.formsGroupsURIMap("risk")
  lazy val fuds = getUserData(user, formGroupUri)
  lazy val fud = fuds(0)
  lazy val instanceURI = fud.data
  lazy val prop = URI("http://www.bizinnov.com/ontologies/quest.owl.ttl#5-1")

  override def beforeAll {
    prepareTDBontologies()
    createEmptyUserData(user)
    responseAnalysis.dataset.rw({
      val graph = (
        instanceURI
        -- prop ->- "2"
        -- prop ->- 3
        -- prop ->- "5"
        a URI("http://www.bizinnov.com/ontologies/quest.owl.ttl#5")).graph
      val res = responseAnalysis.rdfStore.appendToGraph(responseAnalysis.dataset, user.getURI(), graph) match {
        case Success(x) => x
        case Failure(e) => "Failure in appendToGraph " + e
      }
      info(s"after appendToGraph user.getURI() ${user.getURI()} res $res")
    })
  }

  override def afterAll {
    // remove to debug TDB:
    deleteLocalSPARL()
  }

  test("averagePerForm 1") {
    responseAnalysis.dataset.rw({
      val (avg, coefs, _) = responseAnalysis.averagePerForm(user, fromUri(instanceURI))
      info(s""" responseAnalysis ${fud.label}
          avg ${avg} coefs $coefs""")
      assert(avg == 3.3333333f)
    })
  }

  test("global average") {
    val (avg, coefs) = responseAnalysis.averagePerFormGroup(user, formGroupUri)
    info(s""" responseAnalysis averagePerFormGroup ${formGroupUri}
            avg ${avg} coefs $coefs""")
    assert(avg == 3.3333333f)
  }

}