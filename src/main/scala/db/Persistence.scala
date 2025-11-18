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
    val db           = Database("db", usr)
    val usrEncrypted = Crypto.encrypt(s"$usr".getBytes("UTF-8"))
    val contentEnc   = Crypto.encrypt("^".getBytes("UTF-8"))
    for
      _ <- writeBytes("usr.enc", usrEncrypted)
      _ <- writeBytes("db.enc", contentEnc)
    yield db

  def writeBytes(path: String, data: Array[Byte]): Either[Throwable, Unit] =
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

  def readBytes(path: String): Either[Throwable, Array[Byte]] =
    try
      val bytes = Files.readAllBytes(Paths.get(path))
      Right(bytes)
    catch
      case e: Throwable =>
        Left(e)

object Persistence {
  opaque type EntryDictionary = HashMap[String, Entry]

  case class Entry(site: String, title: String, user: User, private val passw: String) {
    def password = DecryptedPassword(EncryptedPassword(passw))
  }

  case class Database(name: String, user: User, entries: EntryDictionary = HashMap.empty):

    def +(entry: Entry): Database =
      copy(entries = entries + (entry.title -> entry))

    def -(title: String): Database =
      copy(entries = entries - title)

    override def toString: String =
      entries.map { case (k, v) => s"$k | ${v.site} | ${v.user}" }.mkString("\n")

    def /(entry: Entry): Either[String, Entry] =
      val optionEntry = entries.find((k, v) => v.title == entry.title)
      optionEntry match {
        case Some((k, v)) => Right(v)
        case None         => Left("Not found")
      }

    def searchEntry(entry: Entry): Either[String, Entry] =
      /(entry)
}
