package mailer

import java.util

import accounting.{BookRecord, DepositWithdrawSpec}
import contracts.BotContractSpec
import user_management.{PasswordResetSpec, UserUpdateSpec}

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

object MailerEngine {
  def main(args: Array[String]) {
    val mailFromClient = new Mail("", "")
    send a new Mail("asd", "asd") ↦

    reply to mailFromClient withMessage "asd" CC "exo@gmail.com" ↪
  }
}

object Mailer {
  def sendDWRequest(username: String, DWSpec: DepositWithdrawSpec) = {}
  def sendDWReply(username: String, DWSpec: DepositWithdrawSpec) = {}
  def sendPasswordResetRequest(username: String, userUpdateSpec: PasswordResetSpec) = {}
  def sendPasswordResetReply(username: String, userUpdateSpec: PasswordResetSpec) = {}
  def sendAddContractRequest(username: String, botContractSpec: BotContractSpec) = {}
  def sendAddContractReply(username: String, botContractSpec: BotContractSpec) = {}
  def sendDataChangeRequest(username: String, userUpdateSpec: UserUpdateSpec) = {}
  def sendDataChangeReply(username: String, userUpdateSpec: UserUpdateSpec) = {}
}