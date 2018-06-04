package helpers

/**
  * Created by spectrum on 6/4/2018.
  */
object Validators {
  def isValidSource(source: String): Boolean = isValidBotName(source) || source.equalsIgnoreCase("manual")

  def isEmail(email: String) = """(\w+)@([\w\.]+)""".r.unapplySeq(email.toLowerCase).isDefined

  def isPhoneNumber(phoneNumber: String) = """^\+\d{5,17}$""".r.unapplySeq(phoneNumber.toLowerCase).isDefined

  def isRegion(region: String) = Seq("arm", "rus", "ukr", "kaz").contains(region.toLowerCase)

  def isRole(role: String) = Seq("admin", "manager", "client").contains(role.toLowerCase)

  def isValidBotName(botName: String) = Seq("alpinist", "cleversniper").contains(botName.toLowerCase)

  def isValidBookName(bookName: String) =  "profit".equals(bookName) || isValidBotName(bookName)

  def isValidType(`type`: String) = `type`.equalsIgnoreCase("deposit") || `type`.equalsIgnoreCase("withdraw")
}
