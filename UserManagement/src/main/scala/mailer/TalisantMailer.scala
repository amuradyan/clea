package mailer

import accounting.DepositWithdrawSpec
import com.typesafe.scalalogging.Logger
import mailer.darling.{InvalidPayloadException, Mail, send}
import user_management.{PasswordResetSpec, User, UserExposed, UserUpdateSpec}

/**
  * Created by spectrum on 5/26/2018.
  */
object TalisantMailer {

  private class TalisantMailer

  private val logger = Logger[TalisantMailer]
  private val talisantSupport = "support@talisant.com"
  private val clea = "clea@talisant.com"

  /**
    * Deposit/Withdraw
    **/
  def sendDWRequestFromCleaToSupport(client: UserExposed, DWSpec: DepositWithdrawSpec) = {
    val body =
      s"""
         |username: ${client.username}
         |operation: ${DWSpec.`type`}
         |amount: ${DWSpec.amount}
         |book: ${DWSpec.bookId}
         |${if (DWSpec.note.nonEmpty) s"note: ${DWSpec.note}"}
      """.stripMargin

    try {
      send a new Mail from clea to talisantSupport withSubject s"${DWSpec.`type`} request" andMessage body darling

      logger.info("A deposit/withdraw request mail from clea to talisant support has been sent")

      sendDWRequestAcceptanceFromSupportToClient(client, DWSpec)
    } catch {
      case e: InvalidPayloadException => {
        logger.error("Unable to send a deposit/withdraw request mail from clea to talisant support")
        logger.error(s"Reason ${e.msg}")
      }
      case _: Throwable => logger.error("Unable to send a deposit/withdraw request mail from clea to talisant support")
    }

  }

  private def sendDWRequestAcceptanceFromSupportToClient(client: UserExposed, DWSpec: DepositWithdrawSpec) = {
    val body =
      s"""
         |Dear ${client.name},
         |
         |Your ${DWSpec.`type`} request has been sent.
         |
         |Request details:
         |
         |operation: ${DWSpec.`type`}
         |amount: ${DWSpec.amount}
         |book: ${DWSpec.bookId}
         |
         |Best,
         |Talisant support
      """.stripMargin

    try {
      send a new Mail from talisantSupport to client.email withSubject s"${DWSpec.`type`} request" andMessage body darling

      logger.info("A deposit/withdraw request mail from support to client has been sent")
    } catch {
      case e: InvalidPayloadException => {
        logger.error("Unable to send a deposit/withdraw request mail from support to client")
        logger.error(s"Reason ${e.msg}")
      }
      case _: Throwable => logger.error("Unable to send a deposit/withdraw request mail from support to client")
    }
  }

  def sendDWRequestApprovalFromSupportToClient(client: UserExposed, DWSpec: DepositWithdrawSpec) = {
    val body =
      s"""
         |Dear ${client.name},
         |
         |Your ${DWSpec.`type`} request has been approved.
         |
         |Request details:
         |
         |amount: ${DWSpec.amount}
         |book: ${DWSpec.bookId}
         |fee: ${DWSpec.fee}
         |
         |${if (DWSpec.note.nonEmpty) s"Note: ${DWSpec.note}"}
         |
         |Best,
         |Talisant support
      """.stripMargin

    try{
      send a new Mail from talisantSupport to client.email withSubject s"${DWSpec.`type`} request" andMessage body darling

      logger.info("A deposit/withdraw request approval mail from support to client has been sent")
    } catch {
      case e: InvalidPayloadException => {
        logger.error("Unable to send a deposit/withdraw request approval mail from support to client")
        logger.error(s"Reason ${e.msg}")
      }
      case _: Throwable => logger.error("Unable to send a deposit/withdraw request approval mail from support to client")
    }
  }

  /**
    * Password reset
    **/
  def sendPasswordResetRequestFromCleaToSupport(client: UserExposed, passwordResetSpec: PasswordResetSpec) = {
    val body =
      s"""
         |username: ${client.username}
         |old password: ${passwordResetSpec.oldPassword}
         |new password: ${passwordResetSpec.newPassword}
         |note: ${passwordResetSpec.note}
      """.stripMargin

    try{
      send a new Mail from clea to talisantSupport withSubject "Password reset request" andMessage body darling

      logger.info("A password reset request mail from clea to support has been sent")
      sendPasswordResetRequestAcceptanceFromSupportToClient(client, passwordResetSpec)
    } catch {
      case e: InvalidPayloadException => {
        logger.error("Unable to send a password reset request mail from clea to support")
        logger.error(s"Reason ${e.msg}")
      }
      case _: Throwable => logger.error("Unable to send a password reset request mail from clea to support")
    }
  }

  private def sendPasswordResetRequestAcceptanceFromSupportToClient(client: UserExposed, passwordResetSpec: PasswordResetSpec) = {
    val body =
      s"""
         |Dear ${client.name},
         |
         |Your password reset request has been sent.
         |
         |Best,
         |Talisant support
      """.stripMargin

    try {
      send a new Mail from talisantSupport to client.email withSubject "Password reset request" andMessage body darling

      logger.info("A password reset request mail from support to client has been sent")
    } catch {
      case e: InvalidPayloadException => {
        logger.error("Unable to send a password reset request mail from support to client")
        logger.error(s"Reason ${e.msg}")
      }
      case _: Throwable => logger.error("Unable to send a password reset request mail from support to client")
    }
  }

  def sendPasswordResetRequestApprovalFromSupportToClient(client: UserExposed, passwordResetSpec: PasswordResetSpec) = {
    val body =
      s"""
         |Dear ${client.name},
         |
         |Your password reset request has been approved.
         |
         |${if (passwordResetSpec.note.nonEmpty) s"Note: ${passwordResetSpec.note}"}
         |
         |Best,
         |Talisant support
      """.stripMargin

    try {
      send a new Mail from talisantSupport to client.email withSubject "Password reset request" andMessage body darling

      logger.info("A password reset request approval mail from support to client has been sent")
    } catch {
      case e: InvalidPayloadException => {
        logger.error("Unable to send a password reset approval request mail from support to client")
        logger.error(s"Reason ${e.msg}")
      }
      case _: Throwable => logger.error("Unable to send a password reset approval request mail from support to client")
    }
  }

  /**
    * Personal data change
    **/
  def sendPersonalDataChangeRequestFromCleaToSupport(client: UserExposed, userUpdateSpec: UserUpdateSpec) = {
    val body =
      s"""
         |username: ${client.name}
         |${if (userUpdateSpec.email.nonEmpty) s"new email: ${userUpdateSpec.email}"}
         |${if (userUpdateSpec.phone.nonEmpty) s"new phone: ${userUpdateSpec.phone}"}
         |${if (userUpdateSpec.email.nonEmpty) s"note: ${userUpdateSpec.note}"}
      """.stripMargin

    try {
      send a new Mail from clea to talisantSupport withSubject "Personal data change request" andMessage body darling

      logger.info("A personal data change request mail from clea to support has been sent")
      sendPersonalDataChangeRequestAcceptanceFromSupportToClient(client, userUpdateSpec)
    } catch {
      case e: InvalidPayloadException => {
        logger.error("Unable to send a personal data change request request mail from clea to support")
        logger.error(s"Reason ${e.msg}")
      }
      case _: Throwable => logger.error("Unable to send a personal data change request request mail from clea to support")
    }
  }

  private def sendPersonalDataChangeRequestAcceptanceFromSupportToClient(client: UserExposed, userUpdateSpec: UserUpdateSpec) = {
    val body =
      s"""
         |Dear ${client.name},
         |
         |Your Personal data change request has been sent.
         |
         |Request details:
         |
         |${if (userUpdateSpec.email.nonEmpty) s"new email: ${userUpdateSpec.email}"}
         |${if (userUpdateSpec.phone.nonEmpty) s"new phone: ${userUpdateSpec.phone}"}
         |
         |Best,
         |Talisant support
      """.stripMargin

    try {
      send a new Mail from talisantSupport to client.email withSubject "Personal data change request" andMessage body darling

      logger.info("A personal data change request mail from support to client has been sent")
    } catch {
      case e: InvalidPayloadException => {
        logger.error("Unable to send a personal data change request request mail from support to client")
        logger.error(s"Reason ${e.msg}")
      }
      case _: Throwable => logger.error("Unable to send a personal data change request request mail from support to client")
    }
  }

  def sendPersonalDataChangeRequestApprovalFromSupportToClient(client: UserExposed, userUpdateSpec: UserUpdateSpec) = {
    val body =
      s"""
         |Dear ${client.name},
         |
         |Your Personal data change request has been approved.
         |
         |Request details:
         |
         |${if (userUpdateSpec.email.nonEmpty) s"new email: ${userUpdateSpec.email}"}
         |${if (userUpdateSpec.phone.nonEmpty) s"new phone: ${userUpdateSpec.phone}"}
         |
         |${if (userUpdateSpec.note.nonEmpty) s"Note: ${userUpdateSpec.note}"}
         |
         |Best,
         |Talisant support
      """.stripMargin

    try {
      send a new Mail from talisantSupport to client.email withSubject "Personal data change request" andMessage body darling

      logger.info("A personal data change request approval mail from support to client has been sent")
    } catch {
      case e: InvalidPayloadException => {
        logger.error("Unable to send a personal data change request request approval mail from support to client")
        logger.error(s"Reason ${e.msg}")
      }
      case _: Throwable => logger.error("Unable to send a personal data change request request approval mail from support to client")
    }
  }
}
