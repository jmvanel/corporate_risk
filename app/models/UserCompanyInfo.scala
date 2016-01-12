package models

/** Stores data from the info form */
case class UserCompanyInfo(val department: Option[String] = None,
    val naf: Option[String] = None,
    val year: Option[String] = None,
    val isGroup: Option[String] = None) {

  def getMap =
    (Map[String, String]() /: this.getClass.getDeclaredFields) { (map, field) =>
      field.setAccessible(true)
      field.get(this).asInstanceOf[Option[String]] match {
        case None => map
        case Some(value) => {
          map + (field.getName -> value)
        }
      }
    }
}
