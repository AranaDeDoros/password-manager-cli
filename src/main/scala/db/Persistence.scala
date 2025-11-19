package org.aranadedoros
package db

import auth.Security.KeyProvider.given_SecretKey
import auth.Security.{Crypto, DecryptedPassword, EncryptedPassword, User}
import db.Persistence.Database

import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.collection.immutable.HashMap

object FileUtils:

  def init(user: String, password: String): Either[Throwable, Database] =
    val usr          = User(user, password)
    val db           = Database()
    val usrEncrypted = Crypto.encrypt(s"$usr".getBytes("UTF-8"))
    val contentEnc   = Crypto.encrypt("^".getBytes("UTF-8"))
    for
      _ <- writeBytes(data = usrEncrypted)
      _ <- writeBytes(data = contentEnc)
    yield db

  def writeBytes(path: String = "usr.enc", data: Array[Byte]): Either[Throwable, Unit] =
    try
      Files.write(
        Paths.get(path),
        data,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      Right(())
    catch
      case e: Throwable =>
        Left(e)

  def readBytes(path: String = "db.enc"): Either[Throwable, Array[Byte]] =
    try
      val bytes = Files.readAllBytes(Paths.get(path))
      Right(bytes)
    catch
      case e: Throwable =>
        Left(e)

object Persistence {

  case class Entry(site: String, title: String, private val passw: String) {
    def password = DecryptedPassword(EncryptedPassword(passw))
  }

  case class Database(name: String = "db", entries: HashMap[String, Entry] = HashMap.empty):

    def +(entry: Entry): Database =
      copy(entries = entries + (entry.title -> entry))

    def -(title: String): Database =
      copy(entries = entries - title)

    def -(entry: Entry): Database =
      copy(entries = entries - entry.title)

    override def toString: String =
      entries.map { case (k, v) => s"$k | ${v.site} | ${v.title}" }.mkString("\n")

    def /(title: String): Either[String, Entry] =
      val optionEntry = entries.find((k, v) => v.title == title)
      optionEntry match {
        case Some((k, v)) => Right(v)
        case None         => Left("Not found")
      }

    def searchEntry(title: String): Either[String, Entry] =
      /(title)
}
