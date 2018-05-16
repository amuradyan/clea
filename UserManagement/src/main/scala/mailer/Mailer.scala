package mailer

/**
  * Created by spectrum on 5/14/2018.
  */

class Mail(from: String, to: String)

object send {
  def a(mail: Mail): MailBuilder = {
    MailBuilder(mail)
  }
}

object reply {
  def to(mail: Mail): MailBuilder = {
    MailBuilder(mail)
  }
}

case class MailBuilder(mail: Mail) {
  def withMessage(message: String): MailBuilder= ???
  def CC(ccEmail: String): MailBuilder = ???
  def ↪ = ???
  def ↦ = ???
}

object Mailer {
  def main(args: Array[String]) {
    val mailFromClient = new Mail("", "")
    send a new Mail("asd", "asd") ↦

    reply to mailFromClient withMessage "asd" CC "exo@gmail.com" ↪
  }
}
