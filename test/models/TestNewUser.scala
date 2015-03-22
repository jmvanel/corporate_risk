package models

import org.scalatest.FunSuite

class TestNewUser extends FunSuite {
  TestCommons.prepareTDBontologies()

  val s = "zz"
  val user = User(s, s)
  UserData.createEmptyUserData(user)
  test("the TDB is populated") {
    println(UserData.getUserData(user))
    val formGroupUri = "http://www.bizinnov.com/ontologies/quest.owl.ttl#capital-fg"
    val userData = UserData.getUserData(user, formGroupUri)
    println(s"${userData.mkString(" , \n")}")
    assert(userData.size == 3)
  }
}
