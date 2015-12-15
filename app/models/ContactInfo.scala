package models

/** Class for contact information for email and phone call request */
case class ContactInfo(
  name: String,
  job: Option[String],
  city: Option[String],
  phone: Option[String],
  email: Option[String],
  message: String)