package models

object TestNewUser extends App {
  //  deductions.runtime.utils.Fil‍eUtils.deleteLocalSPARL()

  val s = "zz"
  val user = User(s, s)
  UserData.createEmptyUserData(user)
  println(UserData.getUserData(user))
}