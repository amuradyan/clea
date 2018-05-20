package mailer

import java.io.{ByteArrayOutputStream, InputStream, InputStreamReader}
import java.util
import java.util.Properties
import javax.mail.Session
import javax.mail.internet.{InternetAddress, MimeMessage}

import accounting.DepositWithdrawSpec
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.Base64
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.gmail.model.Message
import com.google.api.services.gmail.{Gmail, GmailScopes}
import contracts.BotContractSpec
import user_management.{PasswordResetSpec, UserManagement, UserUpdateSpec}

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
  def withMessage(message: String): MailBuilder = ???

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
  val JSON_FACTORY = JacksonFactory.getDefaultInstance()
  var SCOPES = new util.ArrayList[String]()
  SCOPES.add(GmailScopes.GMAIL_COMPOSE)
  SCOPES.add(GmailScopes.GMAIL_SEND)
  val CREDENTIALS_FOLDER = "credentials"

  lazy val clientId: InputStream = getClass.getResourceAsStream("/client_secret.json")
  val HTTP_TRANSPORT = GoogleNetHttpTransport.newTrustedTransport()

  val service = new Gmail.Builder(HTTP_TRANSPORT, JSON_FACTORY, getCredentials)
    .setApplicationName("Clea")
    .build();

  def getCredentials = {
    val clientSecrets = GoogleClientSecrets.load(JSON_FACTORY, new InputStreamReader(clientId))

    val flow = new GoogleAuthorizationCodeFlow.Builder(HTTP_TRANSPORT, JSON_FACTORY, clientSecrets, SCOPES)
      .setDataStoreFactory(new FileDataStoreFactory(new java.io.File(CREDENTIALS_FOLDER)))
      .setAccessType("offline")
      .build()

    new AuthorizationCodeInstalledApp(flow, new LocalServerReceiver()).authorize("clea")
  }

  def createEmail(to: String, from: String, subject: String, bodyText: String) = {
    val props = new Properties()
    val session = Session.getDefaultInstance(props, null)

    val email = new MimeMessage(session)

    email.setFrom(new InternetAddress(from))
    email.addRecipient(javax.mail.Message.RecipientType.TO, new InternetAddress(to))
    email.setSubject(subject)
    email.setText(bodyText)
    email
  }

  def createMessageWithEmail(emailContent: MimeMessage) = {
    val buffer = new ByteArrayOutputStream()
    emailContent.writeTo(buffer)
    val bytes = buffer.toByteArray()
    val encodedEmail = Base64.encodeBase64URLSafeString(bytes)
    val message = new Message()
    message.setRaw(encodedEmail)
    message
  }

  def sendMessage(service: Gmail, userId: String, emailContent: MimeMessage) = {
    var message = createMessageWithEmail(emailContent)
    message = service.users().messages().send(userId, message).execute()

    System.out.println("Message id: " + message.getId())
    System.out.println(message.toPrettyString())
    message
  }

  def sendDWRequest(username: String, DWSpec: DepositWithdrawSpec) = {

//    val text = s"I'd like to ${DWSpec.`type`} ${DWSpec.amount} units ${if(DWSpec.`type`.equals("deposit")) "to" else "from"} ${DWSpec.bookId} ${if (!DWSpec.note.isEmpty) s"\n\n Note:\n${DWSpec.note}"}"
//
//    val email = createEmail("admin@talisant.com", "clea@talisant.com", s"${DWSpec.`type`} request", text)
//    sendMessage(service, "me", email)
  }

  def sendDWReply(username: String, DWSpec: DepositWithdrawSpec) = {
//    val user = UserManagement.getByUsername(username)
//
//    val text = s"Dear ${user.name}\n\n Your ${DWSpec.`type`} ${DWSpec.amount} ${if(DWSpec.`type`.equals("deposit")) "to" else "from"} ${DWSpec.bookId} ${if (!DWSpec.note.isEmpty) s" has been processed.\n\n Note:\n${DWSpec.note}"}"
//    val email = createEmail(user.email, "clea@talisant.com", s"${DWSpec.`type`} reply", text)
//    sendMessage(service, "me", email)
  }

  def sendPasswordResetRequest(username: String, userUpdateSpec: PasswordResetSpec) = {
//    val text = s"I'd like to reset my password"
//
//    val email = createEmail("admin@talisant.com", "clea@talisant.com", "Password reset request", text)
//    sendMessage(service, "me", email)
  }

  def sendPasswordResetReply(username: String, userUpdateSpec: PasswordResetSpec) = {
//    val email = createEmail("admin@talisant.com", "clea@talisant.com", "Password reset reply", "hoparner")
//    sendMessage(service, "me", email)
  }

  def sendAddContractRequest(username: String, botContractSpec: BotContractSpec) = {
//    val user = UserManagement.getByUsername(username)
//    val text = s"I'd like to add a ${botContractSpec.botName} contract with ${botContractSpec.profitMargin}% profit margin. \n\n"
  }

  def sendAddContractReply(username: String, botContractSpec: BotContractSpec) = {
//    val user = UserManagement.getByUsername(username)
//    val text = s"Dear ${user.name}\n\nA contract for ${botContractSpec.botName} with ${botContractSpec.profitMargin}% profit margin has been processed"
//
//    val email = createEmail("admin@talisant.com", "clea@talisant.com", "Contract creation reply", text)
//    sendMessage(service, "me", email)
  }

  def sendDataChangeRequest(username: String, userUpdateSpec: UserUpdateSpec) = {}

  def sendDataChangeReply(username: String, userUpdateSpec: UserUpdateSpec) = {}
}