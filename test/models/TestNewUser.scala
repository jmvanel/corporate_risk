package models

object TestNewUser extends App {
  // TODO set a test TDB, to avoid destroy the default one  
  //  deductions.runtime.utils.FileUtils.deleteLocalSPARL()

  val s = "zz"
  val user = User(s, s)
  UserData.createEmptyUserData(user)
  println(UserData.getUserData(user))
  // TODO assert that the TDB is populated
}
