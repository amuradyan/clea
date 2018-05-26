package mailer

import accounting.DepositWithdrawSpec
import com.typesafe.scalalogging.Logger
import contracts.BotContractSpec
import mailer.darling.{Mail, send}
import user_management.{PasswordResetSpec, UserUpdateSpec}

/**
  * Created by spectrum on 5/26/2018.
  */
object TalisantMailer {

  private class TalisantMailer

  private val logger = Logger[TalisantMailer]

  def sendDWRequest(username: String, DWSpec: DepositWithdrawSpec) = {
    val text = s"I'd like to ${DWSpec.`type`} ${DWSpec.amount} units ${if (DWSpec.`type`.equals("deposit")) "to" else "from"} ${DWSpec.bookId} ${if (!DWSpec.note.isEmpty) s"\n\n Note:\n${DWSpec.note}"}"
    send a new Mail from "admin@talisant.com" to "clea@talisant.com" withSubject "${DWSpec.`type`} request" andMessage text darling
  }

  def sendDWReply(username: String, DWSpec: DepositWithdrawSpec) = ???

  def sendPasswordResetRequest(username: String, userUpdateSpec: PasswordResetSpec) = ???

  def sendPasswordResetReply(username: String, userUpdateSpec: PasswordResetSpec) = ???

  def sendAddContractRequest(username: String, botContractSpec: BotContractSpec) = ???

  def sendAddContractReply(username: String, botContractSpec: BotContractSpec) = ???

  def sendDataChangeRequest(username: String, userUpdateSpec: UserUpdateSpec) = ???

  def sendDataChangeReply(username: String, userUpdateSpec: UserUpdateSpec) = ???
}
